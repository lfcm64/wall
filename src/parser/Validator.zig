const Validator = @This();

const std = @import("std");
const sections = @import("../core/sections.zig");
const types = @import("../core/types.zig");
const indices = @import("../core/indices.zig");

const Parser = @import("Parser.zig");
const Context = @import("Context.zig");
const OperandValidator = @import("Operand.zig");

const Allocator = std.mem.Allocator;
const Section = sections.Section;

const log = std.log.scoped(.validator);

allocator: Allocator,
context: Context = .{},

pub fn init(allocator: Allocator) Validator {
    return .{ .allocator = allocator };
}

pub fn deinit(self: *Validator) void {
    self.context.deinit(self.allocator);
}

pub fn handler(self: *Validator) Parser.ParsingEventHandler(Validator) {
    return .{ .handler = self };
}

pub fn validateTypeSection(self: *Validator, section: Section(.type)) !void {
    log.info("validating type section...", .{});
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
    log.info("type section validated", .{});
}

pub fn validateImportSection(self: *Validator, section: Section(.import)) !void {
    log.info("validating import section...", .{});
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
    log.info("import section validated", .{});
}

pub fn validateFuncSection(self: *Validator, section: Section(.func)) !void {
    log.info("validating func section...", .{});
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
    log.info("func section validated", .{});
}

pub fn validateTableSection(self: *Validator, section: Section(.table)) !void {
    log.info("validating table section...", .{});
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
    log.info("table section validated", .{});
}

pub fn validateMemorySection(self: *Validator, section: Section(.memory)) !void {
    log.info("validating memory section...", .{});
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
    log.info("memory section validated", .{});
}

pub fn validateGlobalSection(self: *Validator, section: Section(.global)) !void {
    log.info("validating global section...", .{});
    const visitor = Section(.global).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, global: types.Global, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                try v.validateGlobal(global);
                try v.context.addGlobal(v.allocator, global);
            }
        }.visit,
    };
    try section.visit(visitor);
    log.info("global section validated", .{});
}

fn validateGlobal(self: *Validator, global: types.Global) !void {
    const init_expr = global.init_expr;
    switch (global.ty.valtype) {
        .i32 => {
            switch (init_expr) {
                .i32 => {},
                .global => |idx| {
                    const src_global = try self.context.getGlobal(idx);
                    if (src_global.mut == .@"var") return error.MutableGlobalNotAllowed;
                    if (src_global.valtype != .i32) return error.InitExprTypeMismatch;
                },
                else => return error.InitExprTypeMismatch,
            }
        },
        .i64 => if (init_expr != .i64) return error.InitExprTypeMismatch,
        .f32 => if (init_expr != .f32) return error.InitExprTypeMismatch,
        .f64 => if (init_expr != .f64) return error.InitExprTypeMismatch,
    }
}

pub fn validateExportSection(self: *Validator, section: Section(.@"export")) !void {
    log.info("validating export section...", .{});
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
    log.info("export section validated", .{});
}

pub fn validateStartSection(self: *Validator, section: Section(.start)) !void {
    log.info("validating start section...", .{});
    if (self.context.funcCount() <= section.func_idx) return error.FuncIndexOutOfBounds;
    log.info("start section validated", .{});
}

pub fn validateElementSection(self: *Validator, section: Section(.elem)) !void {
    log.info("validating element section...", .{});
    const visitor = Section(.elem).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, elem: types.Element, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                if (elem.table_idx >= v.context.tableCount()) return error.TableIndexOutOfBounds;
                try v.validateOffsetExpr(elem.offset);

                var it = elem.indices.iter();
                while (try it.next()) |idx| {
                    if (idx >= v.context.funcCount()) return error.FuncIndexOutOfBounds;
                }
            }
        }.visit,
    };
    try section.visit(visitor);
    log.info("element section validated", .{});
}

pub fn validateCodeSection(self: *Validator, section: Section(.code)) !void {
    log.info("validating code section...", .{});
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
    log.info("code section validated", .{});
}

pub fn validateDataSection(self: *Validator, section: Section(.data)) !void {
    log.info("validating data section...", .{});
    const visitor = Section(.data).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, data: types.Segment, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                try v.validateOffsetExpr(data.offset);

                if (data.mem_idx >= v.context.memoryCount()) return error.MemoryIndexOutOfBounds;
            }
        }.visit,
    };
    try section.visit(visitor);
    log.info("data section validated", .{});
}

fn validateOffsetExpr(self: *Validator, offset: types.Expr) !void {
    switch (offset) {
        .i32 => {},
        .global => |idx| {
            const global = try self.context.getGlobal(idx);
            if (global.valtype != .i32) return error.OffsetExprMustBeI32;
            if (global.mut == .@"var") return error.OffsetExprGlobalMustBeImmutable;
        },
        else => return error.InvalidOffsetConstExpr,
    }
}
