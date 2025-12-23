const FunctionValidator = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const State = @import("State.zig");

const io = std.io;
const types = wasm.types;
const instr = wasm.instr;

const Allocator = std.mem.Allocator;
const ValType = types.ValType;
const Instruction = instr.Instruction;
const Opcode = instr.Opcode;

const OperandStack = std.ArrayList(ValType);
const ControlStack = std.ArrayList(ControlFrame);

const ControlFrame = struct {
    opcode: Opcode = undefined,
    start_types: []const ValType,
    end_types: []const ValType,
    height: usize = 0,
    unreachable_flag: bool = false,
};

const log = std.log.scoped(.func_validator);

allocator: Allocator,

state: *State,
op_stack: OperandStack = .{},
ctrl_stack: ControlStack = .{},

func_type: types.FuncType,
code: []const u8,
locals: []const ValType,

pub fn init(body: types.FuncBody, func_idx: u32, state: *State) !FunctionValidator {
    const func_type = state.typeOfFunc(func_idx);

    var locals: std.ArrayList(ValType) = .{};
    for (func_type.params) |param| try locals.append(state.allocator, param);

    var it = body.locals.iter();
    while (try it.next()) |local| try locals.appendNTimes(state.allocator, local.valtype, local.count);

    return .{
        .allocator = state.allocator,
        .state = state,
        .func_type = func_type,
        .code = body.code,
        .locals = try locals.toOwnedSlice(state.allocator),
    };
}

pub fn deinit(self: *FunctionValidator) void {
    self.allocator.free(self.locals);
    self.op_stack.deinit(self.allocator);
    self.ctrl_stack.deinit(self.allocator);
}

pub fn validate(self: *FunctionValidator) !void {
    try self.pushControlFrame(
        .block,
        self.func_type.params,
        self.func_type.results,
    );

    var it = instr.Iterator.init(self.code);
    while (try it.next()) |in| {
        try self.validateInstr(in);
    }
}

fn pushOperand(self: *FunctionValidator, ty: ValType) !void {
    try self.op_stack.append(self.allocator, ty);
}

fn pushOperands(self: *FunctionValidator, operands: []const ValType) !void {
    for (operands) |op| {
        try self.pushOperand(op);
    }
}

fn popOperand(self: *FunctionValidator) !ValType {
    if (self.op_stack.items.len == 0) return error.OperandStackUnderflow;
    return self.op_stack.pop().?;
}

fn popOperandExpect(self: *FunctionValidator, expected: ValType) !ValType {
    const ty = try self.popOperand();
    if (ty != expected) return error.TypeMismatch;
    return ty;
}

fn popOperandsExpect(self: *FunctionValidator, operands: []const ValType) !void {
    const len = operands.len;
    for (operands, 0..) |_, i| {
        _ = try self.popOperandExpect(operands[len - i - 1]);
    }
}

pub fn pushControlFrame(
    self: *FunctionValidator,
    opcode: Opcode,
    in: []const ValType,
    out: []const ValType,
) !void {
    const frame = ControlFrame{
        .opcode = opcode,
        .start_types = in,
        .end_types = out,
        .height = self.op_stack.items.len,
        .unreachable_flag = false,
    };
    try self.ctrl_stack.append(self.allocator, frame);
    try self.pushOperands(in);
}

fn popControlFrame(self: *FunctionValidator) !ControlFrame {
    if (self.ctrl_stack.items.len == 0) return error.ControlStackUnderflow;
    const frame = self.ctrl_stack.getLast();
    if (!frame.unreachable_flag) {
        try self.popOperandsExpect(frame.end_types);
    }
    if (self.op_stack.items.len != frame.height) return error.ControlFrameMismatchedSizes;
    _ = self.ctrl_stack.pop();
    return frame;
}

fn markUnreachable(self: *FunctionValidator) void {
    if (self.ctrl_stack.items.len == 0) return;
    var frame = &self.ctrl_stack.items[self.ctrl_stack.items.len - 1];
    frame.unreachable_flag = true;
    self.op_stack.items.len = frame.height;
}

fn validateInstr(self: *FunctionValidator, in: Instruction) !void {
    switch (in) {
        // CONTROL
        .block => |block| try self.validateBlockTypeOp(.block, block),
        .loop => |block| try self.validateBlockTypeOp(.loop, block),

        .@"if" => |block| {
            _ = try self.popOperandExpect(.i32);
            try self.validateBlockTypeOp(.@"if", block);
        },
        .@"else" => {
            const frame = try self.popControlFrame();
            if (frame.opcode != .@"if") return error.ElseMustOnlyOccurAfterIf;
            try self.pushControlFrame(.@"else", frame.start_types, frame.end_types);
        },
        .end => {
            const frame = try self.popControlFrame();
            try self.pushOperands(frame.end_types);
        },

        .br => |label| try self.validateBr(label),
        .br_if => |label| try self.validateBrIf(label),
        .br_table => |table| try self.validateBrTable(table),

        .@"return" => {
            const frame = self.ctrl_stack.items[0];
            const operands = if (frame.opcode == .loop) frame.start_types else frame.end_types;
            try self.popOperandsExpect(operands);
            self.markUnreachable();
        },

        .@"unreachable" => self.markUnreachable(),
        .nop => {},

        .call => |idx| {
            const func_type = self.state.typeOfFunc(idx);
            try self.popOperandsExpect(func_type.params);
            try self.pushOperands(func_type.results);
        },
        .call_indirect => |call| {
            _ = try self.popOperandExpect(.i32);
            const func_type = self.state.functypes.items[call.type_idx];
            try self.popOperandsExpect(func_type.params);
            try self.pushOperands(func_type.results);
        },

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
            const loc = self.locals[idx];
            try self.pushOperand(loc);
        },
        .@"local.set" => |idx| {
            const loc = self.locals[idx];
            _ = try self.popOperandExpect(loc);
        },
        .@"local.tee" => |idx| {
            const loc = self.locals[idx];
            _ = try self.popOperandExpect(loc);
            try self.pushOperand(loc);
        },
        .@"global.get" => |idx| {
            const global_type = self.state.globals.items[idx];
            try self.pushOperand(global_type.valtype);
        },
        .@"global.set" => |idx| {
            const global_type = self.state.globals.items[idx];
            _ = try self.popOperandExpect(global_type.valtype);
        },

        // MEMORY LOAD
        .@"i32.load",
        .@"i32.load8_s",
        .@"i32.load8_u",
        .@"i32.load16_s",
        .@"i32.load16_u",
        => try self.validateUnaryOp(.i32, .i32),

        .@"i64.load",
        .@"i64.load8_s",
        .@"i64.load8_u",
        .@"i64.load16_s",
        .@"i64.load16_u",
        .@"i64.load32_s",
        .@"i64.load32_u",
        => try self.validateUnaryOp(.i32, .i64),

        .@"f32.load" => try self.validateUnaryOp(.i32, .f32),
        .@"f64.load" => try self.validateUnaryOp(.i32, .f64),

        // MEMORY STORE
        .@"i32.store",
        .@"i32.store8",
        .@"i32.store16",
        => {
            _ = try self.popOperandExpect(.i32);
            _ = try self.popOperandExpect(.i32);
        },

        .@"i64.store",
        .@"i64.store8",
        .@"i64.store16",
        .@"i64.store32",
        => {
            _ = try self.popOperandExpect(.i64);
            _ = try self.popOperandExpect(.i32);
        },
        .@"f32.store" => {
            _ = try self.popOperandExpect(.f32);
            _ = try self.popOperandExpect(.i32);
        },
        .@"f64.store" => {
            _ = try self.popOperandExpect(.f64);
            _ = try self.popOperandExpect(.i32);
        },

        // MEMORY SIZE / GROW
        .@"memory.size" => try self.pushOperand(.i32),
        .@"memory.grow" => try self.validateUnaryOp(.i32, .i32),

        // CONSTANTS
        .@"i32.const" => try self.pushOperand(.i32),
        .@"i64.const" => try self.pushOperand(.i64),
        .@"f32.const" => try self.pushOperand(.f32),
        .@"f64.const" => try self.pushOperand(.f64),

        // NUMERIC OPS
        .@"i32.eqz" => try self.validateUnaryOp(.i32, .i32),
        .@"i64.eqz" => try self.validateUnaryOp(.i64, .i32),

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
        => try self.validateBinaryOp(.i32, .i32, .i32),

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
        => try self.validateBinaryOp(.i64, .i64, .i32),

        .@"f32.eq",
        .@"f32.ne",
        .@"f32.lt",
        .@"f32.gt",
        .@"f32.le",
        .@"f32.ge",
        => try self.validateBinaryOp(.f32, .f32, .i32),

        .@"f64.eq",
        .@"f64.ne",
        .@"f64.lt",
        .@"f64.gt",
        .@"f64.le",
        .@"f64.ge",
        => try self.validateBinaryOp(.f64, .f64, .i32),

        .@"i32.clz",
        .@"i32.ctz",
        .@"i32.popcnt",
        => try self.validateUnaryOp(.i32, .i32),

        .@"i64.clz",
        .@"i64.ctz",
        .@"i64.popcnt",
        => try self.validateUnaryOp(.i64, .i64),

        .@"f32.abs",
        .@"f32.neg",
        .@"f32.ceil",
        .@"f32.floor",
        .@"f32.trunc",
        .@"f32.nearest",
        .@"f32.sqrt",
        => try self.validateUnaryOp(.f32, .f32),

        .@"f64.abs",
        .@"f64.neg",
        .@"f64.ceil",
        .@"f64.floor",
        .@"f64.trunc",
        .@"f64.nearest",
        .@"f64.sqrt",
        => try self.validateUnaryOp(.f64, .f64),

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
        => try self.validateBinaryOp(.i32, .i32, .i32),

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
        => try self.validateBinaryOp(.i64, .i64, .i64),

        .@"f32.add",
        .@"f32.sub",
        .@"f32.mul",
        .@"f32.div",
        .@"f32.min",
        .@"f32.max",
        .@"f32.copysign",
        => try self.validateBinaryOp(.f32, .f32, .f32),

        .@"f64.add",
        .@"f64.sub",
        .@"f64.mul",
        .@"f64.div",
        .@"f64.min",
        .@"f64.max",
        .@"f64.copysign",
        => try self.validateBinaryOp(.f64, .f64, .f64),

        // CONVERSIONS
        .@"i32.wrap_i64" => try self.validateUnaryOp(.i64, .i32),

        .@"i32.trunc_f32_s",
        .@"i32.trunc_f32_u",
        => try self.validateUnaryOp(.f32, .i32),

        .@"i32.trunc_f64_s",
        .@"i32.trunc_f64_u",
        => try self.validateUnaryOp(.f64, .i32),

        .@"i64.extend_i32_s",
        .@"i64.extend_i32_u",
        => try self.validateUnaryOp(.i32, .i64),

        .@"i64.trunc_f32_s",
        .@"i64.trunc_f32_u",
        => try self.validateUnaryOp(.f32, .i64),

        .@"i64.trunc_f64_s",
        .@"i64.trunc_f64_u",
        => try self.validateUnaryOp(.f64, .i64),

        .@"f32.convert_i32_s",
        .@"f32.convert_i32_u",
        => try self.validateUnaryOp(.i32, .f32),

        .@"f32.convert_i64_s",
        .@"f32.convert_i64_u",
        => try self.validateUnaryOp(.i64, .f32),

        .@"f32.demote_f64" => try self.validateUnaryOp(.f64, .f32),

        .@"f64.convert_i32_s",
        .@"f64.convert_i32_u",
        => try self.validateUnaryOp(.i32, .f64),

        .@"f64.convert_i64_s",
        .@"f64.convert_i64_u",
        => try self.validateUnaryOp(.i64, .f64),

        .@"f64.promote_f32" => try self.validateUnaryOp(.f32, .f64),

        .@"i32.reinterpret_f32" => try self.validateUnaryOp(.f32, .i32),
        .@"i64.reinterpret_f64" => try self.validateUnaryOp(.f64, .i64),
        .@"f32.reinterpret_i32" => try self.validateUnaryOp(.i32, .f32),
        .@"f64.reinterpret_i64" => try self.validateUnaryOp(.i64, .f64),
    }
}

pub fn validateBlockTypeOp(self: *FunctionValidator, opcode: Opcode, block: instr.BlockType) !void {
    std.debug.assert(opcode == .block or opcode == .loop or opcode == .@"if");

    const operands = switch (block) {
        .empty => &[_]ValType{},
        .valtype => &[_]ValType{block.valtype},
    };
    try self.pushControlFrame(opcode, operands, operands);
}

pub fn validateBr(self: *FunctionValidator, label: u32) !void {
    if (label >= self.ctrl_stack.items.len) return error.ValidateBrInvalidLabel;
    const frame = self.ctrl_stack.items[self.ctrl_stack.items.len - 1 - label];

    const operands = labelTypes(frame);
    _ = try self.popOperandsExpect(operands);
    self.markUnreachable();
}

pub fn validateBrIf(self: *FunctionValidator, label: u32) !void {
    if (label >= self.ctrl_stack.items.len) return error.ValidateBrIfInvalidLabel;
    const frame = self.ctrl_stack.items[self.ctrl_stack.items.len - 1 - label];

    const operands = labelTypes(frame);
    _ = try self.popOperandExpect(.i32);
    _ = try self.popOperandsExpect(operands);
    try self.pushOperands(operands);
}

pub fn validateBrTable(self: *FunctionValidator, table: instr.BrTable) !void {
    _ = try self.popOperandExpect(.i32);
    if (table.labels.count >= self.ctrl_stack.items.len) return error.ValidateBrTableInvalidLabel;

    const frame = self.ctrl_stack.items[self.ctrl_stack.items.len - 1 - table.default_label];
    const default_operands = labelTypes(frame);

    var it = table.labels.iter();
    while (try it.next()) |label| {
        if (label >= self.ctrl_stack.items.len) return error.ValidateBrTableInvalidLabelN;
        const frame_n = self.ctrl_stack.items[self.ctrl_stack.items.len - 1 - label];

        const operands = labelTypes(frame_n);
        if (operands.len != default_operands.len) return error.ValidateBrTableInvalidLabelWrongArity;

        if (default_operands.len > 64) return error.TODOAllocation;

        var temp = [_]ValType{.i32} ** 64;
        for (operands, 0..) |_, i| {
            temp[i] = try self.popOperandExpect(operands[default_operands.len - i - 1]);
        }

        for (operands, 0..) |_, i| {
            try self.pushOperand(temp[default_operands.len - 1 - i]);
        }
    }
    try self.popOperandsExpect(default_operands);
    self.markUnreachable();
}

fn validateUnaryOp(self: *FunctionValidator, operand: ValType, result: ValType) !void {
    _ = try self.popOperandExpect(operand);
    try self.pushOperand(result);
}

fn validateBinaryOp(self: *FunctionValidator, left: ValType, right: ValType, result: ValType) !void {
    _ = try self.popOperandExpect(left);
    _ = try self.popOperandExpect(right);
    try self.pushOperand(result);
}

fn labelTypes(frame: ControlFrame) []const ValType {
    if (frame.opcode == Opcode.loop) {
        return frame.start_types;
    } else {
        return frame.end_types;
    }
}
