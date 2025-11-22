const std = @import("std");
const types = @import("module/types.zig");

const Opcode = @import("opcode.zig").Opcode;

const io = std.io;
const Vec = types.primitives.Vec;
const VarU32 = types.primitives.VarU32;

pub const BlockType = union(enum) {
    valtype: types.ValType,
    empty,

    pub fn fromReader(reader: *io.Reader) !BlockType {
        const byte = try reader.takeByte();
        if (byte == 0x40) return .{.empty};

        return .{ .valtype = @enumFromInt(byte) };
    }
};

pub const BrTable = struct {
    labels: Vec(VarU32),
    default_label: u32,

    pub fn fromReader(reader: *io.Reader) !BrTable {
        const initial_pos = reader.seek;
        const count = try reader.takeLeb128(u32);

        for (0..count) |_| {
            _ = try VarU32.fromReader(reader);
        }
        const bytes = reader.buffer[initial_pos..reader.seek];
        const labels = try Vec(VarU32).fromBytes(bytes);

        const default_label = try reader.takeLeb128(u32);
        return .{
            .labels = labels,
            .default_label = default_label,
        };
    }
};

pub const CallIndirect = struct {
    type_idx: u32,
    table_idx: u32,

    pub fn fromReader(reader: *io.Reader) !CallIndirect {
        const type_idx = try reader.takeLeb128(u32);
        const table_idx = try reader.takeLeb128(u32);

        return .{
            .type_idx = type_idx,
            .table_idx = table_idx,
        };
    }
};

pub const MemArg = struct {
    @"align": u32,
    offset: u32,

    pub fn fromReader(reader: *io.Reader) !MemArg {
        const al = try reader.takeLeb128(u32);
        const offset = try reader.takeLeb128(u32);

        return .{
            .@"align" = al,
            .offset = offset,
        };
    }
};

pub const Instruction = union(Opcode) {
    //Control
    @"unreachable",
    nop,
    block: BlockType,
    loop: BlockType,
    @"if": BlockType,
    @"else",
    end,
    br: u32,
    br_if: u32,
    br_table: BrTable,
    @"return",
    call: u32,
    call_indirect: CallIndirect,

    //Parametric
    drop,
    select,

    //Variable
    @"local.get": u32,
    @"local.set": u32,
    @"local.tee": u32,
    @"global.get": u32,
    @"global.set": u32,

    //Memory
    @"i32.load": MemArg,
    @"i64.load": MemArg,
    @"f32.load": MemArg,
    @"f64.load": MemArg,
    @"i32.load8_s": MemArg,
    @"i32.load8_u": MemArg,
    @"i32.load16_s": MemArg,
    @"i32.load16_u": MemArg,
    @"i64.load8_s": MemArg,
    @"i64.load8_u": MemArg,
    @"i64.load16_s": MemArg,
    @"i64.load16_u": MemArg,
    @"i64.load32_s": MemArg,
    @"i64.load32_u": MemArg,

    @"i32.store": MemArg,
    @"i64.store": MemArg,
    @"f32.store": MemArg,
    @"f64.store": MemArg,
    @"i32.store8": MemArg,
    @"i32.store16": MemArg,
    @"i64.store8": MemArg,
    @"i64.store16": MemArg,
    @"i64.store32": MemArg,

    @"memory.size": u32,
    @"memory.grow": u32,

    //Numeric
    @"i32.const": i32,
    @"i64.const": i64,
    @"f32.const": f32,
    @"f64.const": f64,

    @"i32.eqz",
    @"i32.eq",
    @"i32.ne",
    @"i32.lt_s",
    @"i32.lt_u",
    @"i32.gt_s",
    @"i32.gt_u",
    @"i32.le_s",
    @"i32.le_u",
    @"i32.ge_s",
    @"i32.ge_u",
    @"i64.eqz",
    @"i64.eq",
    @"i64.ne",
    @"i64.lt_s",
    @"i64.lt_u",
    @"i64.gt_s",
    @"i64.gt_u",
    @"i64.le_s",
    @"i64.le_u",
    @"i64.ge_s",
    @"i64.ge_u",
    @"f32.eq",
    @"f32.ne",
    @"f32.lt",
    @"f32.gt",
    @"f32.le",
    @"f32.ge",
    @"f64.eq",
    @"f64.ne",
    @"f64.lt",
    @"f64.gt",
    @"f64.le",
    @"f64.ge",

    @"i32.clz",
    @"i32.ctz",
    @"i32.popcnt",
    @"i32.add",
    @"i32.sub",
    @"i32.mul",
    @"i32.div_s",
    @"i32.div_u",
    @"i32.rem_s",
    @"i32.rem_u",
    @"i32.and",
    @"i32.or",
    @"i32.xor",
    @"i32.shl",
    @"i32.shr_s",
    @"i32.shr_u",
    @"i32.rotl",
    @"i32.rotr",

    @"i64.clz",
    @"i64.ctz",
    @"i64.popcnt",
    @"i64.add",
    @"i64.sub",
    @"i64.mul",
    @"i64.div_s",
    @"i64.div_u",
    @"i64.rem_s",
    @"i64.rem_u",
    @"i64.and",
    @"i64.or",
    @"i64.xor",
    @"i64.shl",
    @"i64.shr_s",
    @"i64.shr_u",
    @"i64.rotl",
    @"i64.rotr",

    @"f32.abs",
    @"f32.neg",
    @"f32.ceil",
    @"f32.floor",
    @"f32.trunc",
    @"f32.nearest",
    @"f32.sqrt",
    @"f32.add",
    @"f32.sub",
    @"f32.mul",
    @"f32.div",
    @"f32.min",
    @"f32.max",
    @"f32.copysign",

    @"f64.abs",
    @"f64.neg",
    @"f64.ceil",
    @"f64.floor",
    @"f64.trunc",
    @"f64.nearest",
    @"f64.sqrt",
    @"f64.add",
    @"f64.sub",
    @"f64.mul",
    @"f64.div",
    @"f64.min",
    @"f64.max",
    @"f64.copysign",

    @"i32.wrap_i64",
    @"i32.trunc_f32_s",
    @"i32.trunc_f32_u",
    @"i32.trunc_f64_s",
    @"i32.trunc_f64_u",
    @"i64.extend_i32_s",
    @"i64.extend_i32_u",
    @"i64.trunc_f32_s",
    @"i64.trunc_f32_u",
    @"i64.trunc_f64_s",
    @"i64.trunc_f64_u",
    @"f32.convert_i32_s",
    @"f32.convert_i32_u",
    @"f32.convert_i64_s",
    @"f32.convert_i64_u",
    @"f32.demote_f64",
    @"f64.convert_i32_s",
    @"f64.convert_i32_u",
    @"f64.convert_i64_s",
    @"f64.convert_i64_u",
    @"f64.promote_f32",
    @"i32.reinterpret_f32",
    @"i64.reinterpret_f64",
    @"f32.reinterpret_i32",
    @"f64.reinterpret_i64",

    pub fn fromReader(reader: *io.Reader) !Instruction {
        const byte = try reader.takeByte();
        const opcode: Opcode = @enumFromInt(byte);

        return switch (opcode) {
            // Control (blocktype)
            .block, .loop, .@"if" => @unionInit(
                Instruction,
                @tagName(opcode),
                try BlockType.fromReader(reader),
            ),

            // Control (u32)
            .br, .br_if, .call => @unionInit(
                Instruction,
                @tagName(opcode),
                try reader.takeLeb128(u32),
            ),

            .br_table => .{ .br_table = try BrTable.fromReader(reader) },
            .call_indirect => .{ .call_indirect = try CallIndirect.fromReader(reader) },

            // Variable (u32)
            .@"local.get",
            .@"local.set",
            .@"local.tee",
            .@"global.get",
            .@"global.set",
            => @unionInit(
                Instruction,
                @tagName(opcode),
                try reader.takeLeb128(u32),
            ),

            // Memory (MemArg)
            .@"i32.load",
            .@"i64.load",
            .@"f32.load",
            .@"f64.load",
            .@"i32.load8_s",
            .@"i32.load8_u",
            .@"i32.load16_s",
            .@"i32.load16_u",
            .@"i64.load8_s",
            .@"i64.load8_u",
            .@"i64.load16_s",
            .@"i64.load16_u",
            .@"i64.load32_s",
            .@"i64.load32_u",
            .@"i32.store",
            .@"i64.store",
            .@"f32.store",
            .@"f64.store",
            .@"i32.store8",
            .@"i32.store16",
            .@"i64.store8",
            .@"i64.store16",
            .@"i64.store32",
            => @unionInit(
                Instruction,
                @tagName(opcode),
                try MemArg.fromReader(reader),
            ),

            // Memory (u32)
            .@"memory.size", .@"memory.grow" => @unionInit(
                Instruction,
                @tagName(opcode),
                try reader.takeLeb128(u32),
            ),

            // Numeric constants
            .@"i32.const" => .{ .@"i32.const" = try reader.takeLeb128(i32) },
            .@"i64.const" => .{ .@"i64.const" = try reader.takeLeb128(i64) },
            .@"f32.const" => .{ .@"f32.const" = @bitCast(try reader.takeBytes(4)) },
            .@"f64.const" => .{ .@"f64.const" = @bitCast(try reader.takeBytes(8)) },

            // No immediate
            else => @unionInit(Instruction, @tagName(opcode), {}),
        };
    }
};
