const std = @import("std");
const sections = @import("sections.zig");
const indices = @import("indices.zig");

const io = std.io;
const SectionLimited = sections.SectionLimited;

pub const VarU32 = struct {
    pub fn fromReader(reader: *io.Reader) !u32 {
        return reader.takeLeb128(u32);
    }
};

pub const ValType = enum(u8) {
    i32 = 0x7f,
    i64 = 0x7e,
    f32 = 0x7d,
    f64 = 0x7c,
};

pub const Mutability = enum(u8) {
    @"const" = 0x00,
    @"var" = 0x01,
};

pub const Expr = union(enum) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
    global: indices.GlobalIdx,

    pub fn fromReader(reader: *io.Reader) !Expr {
        const opcode = try reader.takeByte();

        const expr: Expr = switch (opcode) {
            0x41 => .{ .i32 = try reader.takeLeb128(i32) }, // i32.const
            0x42 => .{ .i64 = try reader.takeLeb128(i64) }, // i64.const
            0x43 => .{ .f32 = @floatFromInt(try reader.takeInt(u32, .little)) }, // f32.const
            0x44 => .{ .f64 = @floatFromInt(try reader.takeInt(u64, .little)) }, // f64.const
            0x23 => .{ .global = try reader.takeLeb128(u32) }, // get_global
            else => return error.InvalidInitExprOpcode,
        };

        const end = try reader.takeByte();
        if (end != 0x0B) return error.MissingEndOpcode;
        return expr;
    }
};

pub const Limits = struct {
    min: u32,
    max: ?u32,

    pub fn fromReader(reader: *io.Reader) !Limits {
        const tag = try reader.takeByte();
        const min = try reader.takeLeb128(u32);
        return switch (tag) {
            0x00 => .{
                .min = min,
                .max = null,
            },
            0x01 => .{
                .min = min,
                .max = try reader.takeLeb128(u32),
            },
            else => error.InvalidLimitsTag,
        };
    }
};

pub const Memory = Limits;
pub const Table = Limits;

pub const Local = struct {
    count: u32,
    ty: ValType,

    pub fn fromReader(reader: *io.Reader) !Local {
        return .{
            .count = try reader.takeLeb128(u32),
            .ty = @enumFromInt(try reader.takeByte()),
        };
    }
};

pub const GlobalType = struct {
    ty: ValType,
    mut: Mutability,

    pub fn fromReader(reader: *io.Reader) !GlobalType {
        const ty: ValType = @enumFromInt(try reader.takeByte());
        const mut: Mutability = @enumFromInt(try reader.takeByte());

        return .{
            .val_type = ty,
            .mut = mut,
        };
    }
};

pub const Global = struct {
    ty: GlobalType,
    init_expr: Expr,

    pub fn fromReader(reader: *io.Reader) !Global {
        const ty = try GlobalType.fromReader(reader);
        const init_expr = try Expr.fromReader(reader);

        return Global{
            .ty = ty,
            .init_expr = init_expr,
        };
    }
};

pub const ExportKind = union(enum) {
    func: indices.FuncIdx,
    table: indices.TableIdx,
    memory: indices.MemIdx,
    global: indices.GlobalIdx,

    pub fn fromReader(reader: *io.Reader) !ExportKind {
        const tag = try reader.takeByte();
        const idx = try reader.takeLeb128(u32);
        return switch (tag) {
            0x00 => .{ .func = idx },
            0x01 => .{ .table = idx },
            0x02 => .{ .memory = idx },
            0x03 => .{ .global = idx },
            else => return error.InvalidExportKindTag,
        };
    }
};

pub const Export = struct {
    name: []const u8,
    kind: ExportKind,

    pub fn fromReader(reader: *io.Reader) !Export {
        const name_size = try reader.takeLeb128(u32);
        return .{
            .name = try reader.take(name_size),
            .kind = try ExportKind.fromReader(reader),
        };
    }
};

pub const ImportDesc = union(enum) {
    func: indices.FuncIdx,
    table: Table,
    memory: Memory,
    global: GlobalType,
};

pub const Import = struct {
    module: []const u8,
    name: []const u8,
    desc: ImportDesc,

    pub fn fromReader(reader: *io.Reader) !Import {
        const tag = try reader.takeByte();

        const module_size = try reader.takeLeb128(u32);
        const module = try reader.take(module_size);

        const name_size = try reader.takeLeb128(u32);
        const name = try reader.take(name_size);

        const desc: ImportDesc = switch (tag) {
            0x00 => .{ .func = try reader.takeLeb128(u32) },
            0x01 => .{ .table = try Table.fromReader(reader) },
            0x02 => .{ .memory = try Memory.fromReader(reader) },
            0x03 => .{ .global = try GlobalType.fromReader(reader) },
            else => return error.InvalidImportDescTag,
        };
        return .{
            .module = module,
            .name = name,
            .desc = desc,
        };
    }
};

pub const ResultType = []const ValType;

pub const FuncType = struct {
    params: ResultType,
    results: ResultType,

    pub fn fromReader(reader: *io.Reader) !FuncType {
        const tag = try reader.takeByte();
        if (tag != 0x60) return error.InvalidFuncTypeTag;

        const param_count = try reader.takeLeb128(u32);
        const params: ResultType = @ptrCast(try reader.take(param_count));

        const result_count = try reader.takeLeb128(u32);
        const results: ResultType = @ptrCast(try reader.take(result_count));

        return .{
            .params = params,
            .results = results,
        };
    }
};

pub const FuncBody = struct {
    const Locals = SectionLimited(Local, Local.fromReader);

    size: u32,
    locals: Locals,
    code: []const u8,

    pub fn fromReader(reader: *io.Reader) !FuncBody {
        const size = try reader.takeLeb128(u32);

        const initial_pos = reader.seek;
        const local_count = try reader.takeLeb128(u32);

        for (0..local_count) |_| {
            _ = try Local.fromReader(reader);
        }
        const bytes = reader.buffer[initial_pos..reader.seek];
        const locals = try Locals.fromBytes(bytes);

        const code_size = size - (reader.seek - initial_pos);
        const code = try reader.take(code_size);

        return .{
            .size = size,
            .locals = locals,
            .code = code,
        };
    }
};

pub const Element = struct {
    const Indices = SectionLimited(u32, VarU32.fromReader);

    table_idx: indices.TableIdx,
    offset: Expr,
    indices: Indices,

    pub fn fromReader(reader: *io.Reader) !Element {
        const table_idx = try reader.takeLeb128(u32);
        const offset = try Expr.fromReader(reader);

        const initial_pos = reader.seek;
        const index_count = try reader.takeLeb128(u32);

        for (0..index_count) |_| {
            _ = try VarU32.fromReader(reader);
        }
        const bytes = reader.buffer[initial_pos..reader.seek];
        return .{
            .table_idx = table_idx,
            .offset = offset,
            .indices = try Indices.fromBytes(bytes),
        };
    }
};

pub const Segment = struct {
    memory_idx: indices.MemIdx,
    offset: Expr,
    bytes: []const u8,

    pub fn fromReader(reader: *io.Reader) !Segment {
        const memory_idx = try reader.takeLeb128(u32);
        const offset = try Expr.fromReader(reader);

        const bytes_size = try reader.takeLeb128(u32);
        const bytes: []const u8 = @ptrCast(try reader.take(bytes_size));

        return Segment{
            .memory_idx = memory_idx,
            .offset = offset,
            .bytes = bytes,
        };
    }
};
