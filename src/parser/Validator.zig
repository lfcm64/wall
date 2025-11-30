const Validator = @This();

const std = @import("std");
const sections = @import("../core/sections.zig");
const types = @import("../core/types.zig");
const indices = @import("../core/indices.zig");

const Parser = @import("Parser.zig");
const Context = @import("Context.zig");
const OperandValidator = @import("Operand.zig");

const Allocator = std.mem.Allocator;
const Payload = Parser.Payload;
const Section = sections.Section;

allocator: Allocator,
parser: Parser,
context: Context = .{},

pub fn init(allocator: Allocator, bytes: []const u8) Validator {
    return .{
        .allocator = allocator,
        .parser = Parser.init(bytes),
    };
}

pub fn deinit(self: *Validator) void {
    self.context.deinit(self.allocator);
}

pub fn validateNext(self: *Validator) !void {
    while (try self.parser.parseNext()) |payload| switch (payload) {
        .module_header => |header| {
            if (header.magic != 0x6D736100) return error.WASMMagicError;
            if (header.version != 1) return error.VersionMismatch;
        },
        .custom_section => {},
        .type_section => |section| try self.validateTypeSection(section),
        .import_section => |section| try self.validateImportSection(section),
        .function_section => |section| try self.validateFuncSection(section),
        .table_section => |section| try self.validateTableSection(section),
        .memory_section => |section| try self.validateMemorySection(section),
        .global_section => |section| try self.validateGlobalSection(section),
        .export_section => |section| try self.validateExportSection(section),
        .start_section => |section| try self.validateStartSection(section),
        .element_section => |section| try self.validateElementSection(section),
        .code_section => |section| try self.validateCodeSection(section),
        .data_section => |section| try self.validateDataSection(section),
    };
}

fn validateTypeSection(self: *Validator, section: Section(.type)) !void {
    const visitor = Section(.type).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, functype: types.FuncType, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                try v.context.addFuncType(v.allocator, functype);
            }
        }.visit,
    };
    try section.visit(visitor);
}

fn validateImportSection(self: *Validator, section: Section(.import)) !void {
    const visitor = Section(.import).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, import: types.Import, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                try v.context.addImport(v.allocator, import);
            }
        }.visit,
    };
    try section.visit(visitor);
}

fn validateFuncSection(self: *Validator, section: Section(.func)) !void {
    const visitor = Section(.func).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, func: indices.FuncIdx, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                if (v.context.funcTypeCount() <= func) return error.FuncIndexOutOfBounds;

                try v.context.addFunc(v.allocator, func);
            }
        }.visit,
    };
    try section.visit(visitor);
}

fn validateTableSection(self: *Validator, section: Section(.table)) !void {
    const visitor = Section(.table).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, table: types.Table, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                try v.context.addTable(v.allocator, table);
            }
        }.visit,
    };
    try section.visit(visitor);
}

fn validateMemorySection(self: *Validator, section: Section(.memory)) !void {
    const visitor = Section(.memory).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, mem: types.Memory, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                try v.context.addMemory(v.allocator, mem);
            }
        }.visit,
    };
    try section.visit(visitor);
}

fn validateGlobalSection(self: *Validator, section: Section(.global)) !void {
    const visitor = Section(.global).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, global: types.Global, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));

                const init_expr = global.init_expr;

                switch (global.ty.ty) {
                    .i32 => {
                        switch (init_expr) {
                            .i32 => {},
                            .global => |src_idx| {
                                const src_global = v.context.globals.items[src_idx];
                                if (src_global.mut == .@"var") return error.MutableGlobalNotAllowed;
                                if (src_global.ty != .i32) return error.InitExprTypeMismatch;
                            },
                            else => return error.InitExprTypeMismatch,
                        }
                    },
                    .i64 => if (init_expr != .i64) return error.InitExprTypeMismatch,
                    .f32 => if (init_expr != .f32) return error.InitExprTypeMismatch,
                    .f64 => if (init_expr != .f64) return error.InitExprTypeMismatch,
                }
                try v.context.addGlobal(v.allocator, global);
            }
        }.visit,
    };
    try section.visit(visitor);
}

fn validateExportSection(self: *Validator, section: Section(.@"export")) !void {
    const visitor = Section(.@"export").Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, exp: types.Export, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                switch (exp.kind) {
                    .func => |idx| if (v.context.funcCount() <= idx) return error.FuncIndexOutOfBounds,
                    .global => |idx| if (v.context.globalCount() <= idx) return error.GlobalIndexOutOfBounds,
                    .memory => |idx| if (v.context.memoryCount() <= idx) return error.MemIndexOutOfBounds,
                    .table => |idx| if (v.context.tableCount() <= idx) return error.TableIndexOutOfBounds,
                }
                try v.context.addExport(v.allocator, exp);
            }
        }.visit,
    };
    try section.visit(visitor);
}

fn validateStartSection(self: *Validator, section: Section(.start)) !void {
    if (self.context.funcCount() <= section.func_idx) return error.FuncIndexOutOfBounds;
}

fn validateElementSection(self: *Validator, section: Section(.elem)) !void {
    const visitor = Section(.elem).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, elem: types.Element, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                if (elem.table_idx >= v.context.tableCount()) return error.TableIndexOutOfBounds;

                switch (elem.offset) {
                    .i32, .global => {},
                    else => return error.InvalidOffsetConstExpr,
                }
                var it = elem.indices.iter();
                while (try it.next()) |idx| {
                    if (idx >= v.context.funcCount()) return error.FuncIndexOutOfBounds;
                }
            }
        }.visit,
    };
    try section.visit(visitor);
}

fn validateCodeSection(self: *Validator, section: Section(.code)) !void {
    const visitor = Section(.code).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, body: types.FuncBody, idx: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                var operand = try OperandValidator.init(
                    v.allocator,
                    &v.context,
                    body,
                    idx,
                );
                defer operand.deinit();
                try operand.validate();
            }
        }.visit,
    };
    try section.visit(visitor);
}

fn validateDataSection(self: *Validator, section: Section(.data)) !void {
    const visitor = Section(.data).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, data: types.Segment, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                if (data.mem_idx >= v.context.memoryCount()) return error.MemoryIndexOutOfBounds;

                switch (data.offset) {
                    .i32 => {},
                    .global => |idx| {
                        if (idx >= v.context.globalCount()) return error.GlobalIndexOutOfBounds;

                        const global = v.context.globals.items[idx];
                        if (global.ty != .i32) return error.OffsetExprMustBeI32;
                        if (global.mut == .@"var") return error.OffsetExprGlobalMustBeImmutable;
                    },
                    else => return error.InvalidOffsetConstExpr,
                }
            }
        }.visit,
    };
    try section.visit(visitor);
}
