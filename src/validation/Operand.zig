const OperandValidator = @This();

const std = @import("std");
const types = @import("../module/types.zig");

const Context = @import("Context.zig");
const Opcode = @import("../opcode.zig").Opcode;
const Instruction = @import("../instruction.zig").Instruction;

const io = std.io;
const Allocator = std.mem.Allocator;
const ValType = types.ValType;
const OperandStack = std.ArrayList(ValType);
const ControlStack = std.ArrayList(ControlFrame);

const ControlFrame = struct {
    opcode: Opcode = undefined,
    start_types: []const ValType,
    end_types: []const ValType,
    height: usize = 0,
    unreachable_flag: bool = false,
};

allocator: Allocator,
reader: io.Reader,

context: *Context,

op_stack: OperandStack = .{},
ctrl_stack: ControlStack = .{},

locals: std.ArrayList(ValType),

pub fn init(allocator: Allocator, context: *Context, func: types.Function, func_idx: u32) !OperandValidator {
    const locals = try expandAllLocals(allocator, context, func, func_idx);
    return .{
        .allocator = allocator,
        .reader = io.Reader.fixed(func.code),
        .context = context,
        .locals = locals,
    };
}

fn expandAllLocals(
    allocator: Allocator,
    context: *Context,
    func: types.Function,
    func_idx: u32,
) !std.ArrayList(ValType) {
    var locals: std.ArrayList(ValType) = .{};

    // 1. Get function type from store
    const func_type = context.getFuncTypeByFuncIdx(func_idx);

    // 2. Add parameters first
    for (func_type.params) |p| {
        try locals.append(allocator, p);
    }

    // 3. Now expand local declarations
    var it = func.locals.iter();
    while (try it.next()) |local| {
        for (0..local.count) |_| {
            try locals.append(allocator, local.val_type);
        }
    }
    return locals;
}

pub fn deinit(self: *OperandValidator) void {
    self.locals.deinit(self.allocator);
    self.op_stack.deinit(self.allocator);
    self.ctrl_stack.deinit(self.allocator);
}

pub fn validate(self: *OperandValidator) !void {
    while (self.reader.seek < self.reader.buffer.len) {
        const instr = try Instruction.fromReader(&self.reader);
        try self.validateInstruction(instr);
    }
}

fn pushOperand(self: *OperandValidator, ty: ValType) !void {
    try self.op_stack.append(self.allocator, ty);
}

fn popOperand(self: *OperandValidator) !ValType {
    if (self.op_stack.items.len == 0) return error.StackUnderflow;
    return self.op_stack.pop().?;
}

fn popOperandExpect(self: *OperandValidator, expected: ValType) !void {
    const t = try self.popOperand();
    if (t != expected) return error.TypeMismatch;
}

fn validateInstruction(self: *OperandValidator, instr: Instruction) !void {
    switch (instr) {
        // CONTROL
        .block,
        .loop,
        .@"if",
        .@"else",
        .end,
        .br,
        .br_if,
        .br_table,
        .@"return",
        .@"unreachable",
        .nop,
        => {},

        .call => |idx| {
            const func_type = self.context.getFuncTypeByFuncIdx(idx);

            for (func_type.params) |param| {
                try self.popOperandExpect(param);
            }
            for (func_type.results) |res| {
                try self.pushOperand(res);
            }
        },
        .call_indirect => {},

        // PARAMETRIC
        .drop => _ = try self.popOperand(),
        .select => {
            _ = try self.popOperandExpect(.i32);
            const t2 = try self.popOperand();
            const t1 = try self.popOperand();

            if (t1 != t2) return error.SelectTypeMismatch;
            try self.pushOperand(t1);
        },

        // VARIABLES
        .@"local.get" => |idx| {
            const local = self.locals.items[idx];
            try self.pushOperand(local);
        },
        .@"local.set" => |idx| {
            const local = self.locals.items[idx];
            try self.popOperandExpect(local);
        },
        .@"local.tee" => |idx| {
            const local = self.locals.items[idx];
            try self.popOperandExpect(local);
            try self.pushOperand(local);
        },
        .@"global.get" => |idx| {
            const global = self.context.globals.items[idx];
            try self.pushOperand(global.val_type);
        },
        .@"global.set" => |idx| {
            const global = self.context.globals.items[idx];
            try self.popOperandExpect(global.val_type);
        },

        // MEMORY LOAD
        .@"i32.load",
        .@"i32.load8_s",
        .@"i32.load8_u",
        .@"i32.load16_s",
        .@"i32.load16_u",
        => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.i32);
        },
        .@"i64.load",
        .@"i64.load8_s",
        .@"i64.load8_u",
        .@"i64.load16_s",
        .@"i64.load16_u",
        .@"i64.load32_s",
        .@"i64.load32_u",
        => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.i64);
        },
        .@"f32.load" => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.f32);
        },
        .@"f64.load" => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.f64);
        },

        // MEMORY STORE
        .@"i32.store", .@"i32.store8", .@"i32.store16" => {
            try self.popOperandExpect(.i32);
            try self.popOperandExpect(.i32);
        },
        .@"i64.store",
        .@"i64.store8",
        .@"i64.store16",
        .@"i64.store32",
        => {
            try self.popOperandExpect(.i64);
            try self.popOperandExpect(.i32);
        },
        .@"f32.store" => {
            try self.popOperandExpect(.f32);
            try self.popOperandExpect(.i32);
        },
        .@"f64.store" => {
            try self.popOperandExpect(.f64);
            try self.popOperandExpect(.i32);
        },

        // MEMORY SIZE / GROW
        .@"memory.size" => try self.pushOperand(.i32),
        .@"memory.grow" => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.i32);
        },

        // CONSTANTS
        .@"i32.const" => try self.pushOperand(.i32),
        .@"i64.const" => try self.pushOperand(.i64),
        .@"f32.const" => try self.pushOperand(.f32),
        .@"f64.const" => try self.pushOperand(.f64),

        // NUMERIC OPS
        .@"i32.eqz" => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.i32);
        },

        .@"i64.eqz" => {
            try self.popOperandExpect(.i64);
            try self.pushOperand(.i32);
        },
        .@"i32.eq",
        .@"i32.ne",
        .@"i32.lt_s",
        .@"i32.lt_u",
        .@"i32.gt_s",
        .@"i32.gt_u",
        .@"i32.le_s",
        .@"i32.le_u",
        .@"i32.ge_s",
        .@"i32.ge_u",
        => {
            try self.popOperandExpect(.i32);
            try self.popOperandExpect(.i32);
            try self.pushOperand(.i32);
        },
        .@"i64.eq",
        .@"i64.ne",
        .@"i64.lt_s",
        .@"i64.lt_u",
        .@"i64.gt_s",
        .@"i64.gt_u",
        .@"i64.le_s",
        .@"i64.le_u",
        .@"i64.ge_s",
        .@"i64.ge_u",
        => {
            try self.popOperandExpect(.i64);
            try self.popOperandExpect(.i64);
            try self.pushOperand(.i32);
        },
        .@"f32.eq",
        .@"f32.ne",
        .@"f32.lt",
        .@"f32.gt",
        .@"f32.le",
        .@"f32.ge",
        => {
            try self.popOperandExpect(.f32);
            try self.popOperandExpect(.f32);
            try self.pushOperand(.i32);
        },
        .@"f64.eq",
        .@"f64.ne",
        .@"f64.lt",
        .@"f64.gt",
        .@"f64.le",
        .@"f64.ge",
        => {
            try self.popOperandExpect(.f64);
            try self.popOperandExpect(.f64);
            try self.pushOperand(.i32);
        },
        .@"i32.clz", .@"i32.ctz", .@"i32.popcnt" => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.i32);
        },
        .@"i64.clz", .@"i64.ctz", .@"i64.popcnt" => {
            try self.popOperandExpect(.i64);
            try self.pushOperand(.i64);
        },
        .@"f32.abs",
        .@"f32.neg",
        .@"f32.ceil",
        .@"f32.floor",
        .@"f32.trunc",
        .@"f32.nearest",
        .@"f32.sqrt",
        => {
            try self.popOperandExpect(.f32);
            try self.pushOperand(.f32);
        },
        .@"f64.abs",
        .@"f64.neg",
        .@"f64.ceil",
        .@"f64.floor",
        .@"f64.trunc",
        .@"f64.nearest",
        .@"f64.sqrt",
        => {
            try self.popOperandExpect(.f64);
            try self.pushOperand(.f64);
        },
        .@"i32.add",
        .@"i32.sub",
        .@"i32.mul",
        .@"i32.div_s",
        .@"i32.div_u",
        .@"i32.rem_s",
        .@"i32.rem_u",
        .@"i32.and",
        .@"i32.or",
        .@"i32.xor",
        .@"i32.shl",
        .@"i32.shr_s",
        .@"i32.shr_u",
        .@"i32.rotl",
        .@"i32.rotr",
        => {
            try self.popOperandExpect(.i32);
            try self.popOperandExpect(.i32);
            try self.pushOperand(.i32);
        },
        .@"i64.add",
        .@"i64.sub",
        .@"i64.mul",
        .@"i64.div_s",
        .@"i64.div_u",
        .@"i64.rem_s",
        .@"i64.rem_u",
        .@"i64.and",
        .@"i64.or",
        .@"i64.xor",
        .@"i64.shl",
        .@"i64.shr_s",
        .@"i64.shr_u",
        .@"i64.rotl",
        .@"i64.rotr",
        => {
            try self.popOperandExpect(.i64);
            try self.popOperandExpect(.i64);
            try self.pushOperand(.i64);
        },
        .@"f32.add",
        .@"f32.sub",
        .@"f32.mul",
        .@"f32.div",
        .@"f32.min",
        .@"f32.max",
        .@"f32.copysign",
        => {
            try self.popOperandExpect(.f32);
            try self.popOperandExpect(.f32);
            try self.pushOperand(.f32);
        },
        .@"f64.add",
        .@"f64.sub",
        .@"f64.mul",
        .@"f64.div",
        .@"f64.min",
        .@"f64.max",
        .@"f64.copysign",
        => {
            try self.popOperandExpect(.f64);
            try self.popOperandExpect(.f64);
            try self.pushOperand(.f64);
        },

        // CONVERSIONS
        .@"i32.wrap_i64" => {
            try self.popOperandExpect(.i64);
            try self.pushOperand(.i32);
        },
        .@"i32.trunc_f32_s", .@"i32.trunc_f32_u" => {
            try self.popOperandExpect(.f32);
            try self.pushOperand(.i32);
        },
        .@"i32.trunc_f64_s", .@"i32.trunc_f64_u" => {
            try self.popOperandExpect(.f64);
            try self.pushOperand(.i32);
        },
        .@"i64.extend_i32_s", .@"i64.extend_i32_u" => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.i64);
        },
        .@"i64.trunc_f32_s", .@"i64.trunc_f32_u" => {
            try self.popOperandExpect(.f32);
            try self.pushOperand(.i64);
        },
        .@"i64.trunc_f64_s", .@"i64.trunc_f64_u" => {
            try self.popOperandExpect(.f64);
            try self.pushOperand(.i64);
        },

        .@"f32.convert_i32_s", .@"f32.convert_i32_u" => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.f32);
        },
        .@"f32.convert_i64_s", .@"f32.convert_i64_u" => {
            try self.popOperandExpect(.i64);
            try self.pushOperand(.f32);
        },
        .@"f32.demote_f64" => {
            try self.popOperandExpect(.f64);
            try self.pushOperand(.f32);
        },

        .@"f64.convert_i32_s", .@"f64.convert_i32_u" => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.f64);
        },
        .@"f64.convert_i64_s", .@"f64.convert_i64_u" => {
            try self.popOperandExpect(.i64);
            try self.pushOperand(.f64);
        },
        .@"f64.promote_f32" => {
            try self.popOperandExpect(.f32);
            try self.pushOperand(.f64);
        },

        .@"i32.reinterpret_f32" => {
            try self.popOperandExpect(.f32);
            try self.pushOperand(.i32);
        },
        .@"i64.reinterpret_f64" => {
            try self.popOperandExpect(.f64);
            try self.pushOperand(.i64);
        },
        .@"f32.reinterpret_i32" => {
            try self.popOperandExpect(.i32);
            try self.pushOperand(.f32);
        },
        .@"f64.reinterpret_i64" => {
            try self.popOperandExpect(.i64);
            try self.pushOperand(.f64);
        },
    }
}
