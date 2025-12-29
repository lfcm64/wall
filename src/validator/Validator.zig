const Validator = @This();

const std = @import("std");
const wasm = @import("wasm");
const code = @import("code.zig");

const Context = @import("Context.zig");
const ParsingEvent = @import("../parser/event.zig").Event;

const types = wasm.types;
const sections = wasm.sections;
const indices = wasm.indices;

const Allocator = std.mem.Allocator;
const Section = sections.Section;

const log = std.log.scoped(.validator);

ctx: Context,

pub fn init(allocator: Allocator) Validator {
    return .{ .ctx = Context.init(allocator) };
}

pub fn deinit(self: *Validator) void {
    self.ctx.deinit();
}

pub fn onEvent(validator: *Validator, event: ParsingEvent) !void {
    switch (event) {
        .type_section => |section| try validator.validateTypeSection(section),
        .import_section => |section| try validator.validateImportSection(section),
        .func_section => |section| try validator.validateFuncSection(section),
        .table_section => |section| try validator.validateTableSection(section),
        .memory_section => |section| try validator.validateMemorySection(section),
        .global_section => |section| try validator.validateGlobalSection(section),
        .export_section => |section| try validator.validateExportSection(section),
        .start_section => |section| try validator.validateStartSection(section),
        .element_section => |section| try validator.validateElementSection(section),
        .code_section => |section| try validator.validateCodeSection(section),
        .data_section => |section| try validator.validateDataSection(section),
        else => {},
    }
}

fn ItemValidator(comptime section_type: sections.SectionType) type {
    return *const fn (ctx: *Context, item: sections.SectionItem(section_type), idx: u32) anyerror!void;
}

fn validateEach(
    self: *Validator,
    comptime section_type: sections.SectionType,
    section: Section(section_type),
    validate_item: ItemValidator(section_type),
) !void {
    var it = section.iter();
    var i: u32 = 0;
    while (try it.next()) |item| : (i += 1) {
        try validate_item(&self.ctx, item, i);
    }
}

fn validateTypeSection(self: *Validator, section: Section(.type)) !void {
    try self.validateEach(
        .type,
        section,
        struct {
            fn validate(ctx: *Context, ty: types.FuncType, _: u32) !void {
                try ctx.addFuncType(ty);
            }
        }.validate,
    );
}

fn validateImportSection(self: *Validator, section: Section(.import)) !void {
    try self.validateEach(
        .import,
        section,
        struct {
            fn validate(ctx: *Context, import: types.Import, _: u32) !void {
                switch (import.desc) {
                    .func => |type_idx| if (type_idx >= ctx.functypes.items.len) return error.FuncIndexOutOfBounds,
                    .table => |table| try verifyTable(table),
                    .memory => |mem| try verifyMemory(mem),
                    .global => |global_ty| if (global_ty.mut != .@"const") return error.ImportedGlobalMustBeImmutable,
                }
                try ctx.addImport(import);
            }
        }.validate,
    );
}

fn validateFuncSection(self: *Validator, section: Section(.func)) !void {
    try self.validateEach(
        .func,
        section,
        struct {
            fn validate(ctx: *Context, type_idx: indices.Func, _: u32) !void {
                if (type_idx >= ctx.functypes.items.len) return error.FuncIndexOutOfBounds;
                try ctx.addFunc(type_idx);
            }
        }.validate,
    );
}

fn validateTableSection(self: *Validator, section: Section(.table)) !void {
    try self.validateEach(
        .table,
        section,
        struct {
            fn validate(ctx: *Context, table: types.Table, _: u32) !void {
                try verifyTable(table);
                try ctx.addTable(table);
            }
        }.validate,
    );
}

fn validateMemorySection(self: *Validator, section: Section(.memory)) !void {
    try self.validateEach(
        .memory,
        section,
        struct {
            fn validate(ctx: *Context, mem: types.Memory, _: u32) !void {
                try verifyMemory(mem);
                try ctx.addMemory(mem);
            }
        }.validate,
    );
}

fn validateGlobalSection(self: *Validator, section: Section(.global)) !void {
    try self.validateEach(
        .global,
        section,
        struct {
            fn validate(ctx: *Context, global: types.Global, _: u32) !void {
                try validateConstExpr(ctx, global.init_expr, global.ty.valtype);
                try ctx.addGlobal(global);
            }
        }.validate,
    );
}

fn validateExportSection(self: *Validator, section: Section(.@"export")) !void {
    try self.validateEach(
        .@"export",
        section,
        struct {
            fn validate(ctx: *Context, exp: types.Export, _: u32) !void {
                switch (exp.kind) {
                    .func => |func_idx| if (func_idx >= ctx.funcs.items.len) return error.ExportFuncIndexOutOfBounds,
                    .table => |table_idx| if (table_idx >= ctx.tables.items.len) return error.ExportTableIndexOutOfBounds,
                    .memory => |mem_idx| if (mem_idx >= ctx.memories.items.len) return error.ExportMemoryIndexOutOfBounds,
                    .global => |global_idx| if (global_idx >= ctx.globals.items.len) return error.ExportGlobalIndexOutOfBounds,
                }
                try ctx.addExport(exp);
            }
        }.validate,
    );
}

fn validateStartSection(self: *Validator, section: Section(.start)) !void {
    if (section.func_idx >= self.ctx.funcs.items.len) return error.StartFuncIndexOutOfBounds;
}

fn validateElementSection(self: *Validator, section: Section(.elem)) !void {
    try self.validateEach(
        .elem,
        section,
        struct {
            fn validate(ctx: *Context, elem: types.Element, _: u32) !void {
                if (elem.table_idx >= ctx.tables.items.len) return error.TableIndexOutOfBounds;
                try validateConstExpr(ctx, elem.offset, .i32);

                const func_count = ctx.funcs.items.len;
                var it = elem.indices.iter();
                while (try it.next()) |func_idx| if (func_idx >= func_count) return error.ElementFuncIndexOutOfBounds;
            }
        }.validate,
    );
}

fn validateCodeSection(self: *Validator, section: Section(.code)) !void {
    try self.validateEach(.code, section, code.validateCode);
}

fn validateDataSection(self: *Validator, section: Section(.data)) !void {
    try self.validateEach(
        .data,
        section,
        struct {
            fn validate(ctx: *Context, data: types.Segment, _: u32) !void {
                if (data.mem_idx >= ctx.memories.items.len) return error.MemoryIndexOutOfBounds;
                try validateConstExpr(ctx, data.offset, .i32);
            }
        }.validate,
    );
}

fn validateConstExpr(ctx: *Context, expr: types.Expr, expected_type: types.ValType) !void {
    switch (expr) {
        .i32 => if (expected_type != .i32) return error.ConstExprTypeMismatch,
        .i64 => if (expected_type != .i64) return error.ConstExprTypeMismatch,
        .f32 => if (expected_type != .f32) return error.ConstExprTypeMismatch,
        .f64 => if (expected_type != .f64) return error.ConstExprTypeMismatch,
        .global => |global_idx| {
            if (global_idx >= ctx.imported_globals) return error.ConstExprReferencesNonImportedGlobal;

            const global = ctx.globals.items[global_idx];
            if (global.mut != .@"const") return error.ConstExprReferenceMutableGlobal;
            if (global.valtype != expected_type) return error.ConstExprTypeMismatch;
        },
    }
}

fn verifyTable(table: types.Table) !void {
    if (table.limits.max) |max| {
        if (table.limits.min > max) return error.InvalidTableLimits;
    }
}

fn verifyMemory(mem: types.Memory) !void {
    if (mem.max) |max| {
        if (mem.min > max) return error.InvalidMemoryLimits;
    }
}
