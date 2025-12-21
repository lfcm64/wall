const Validator = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const Context = @import("../parser/Context.zig");
const Event = @import("../parser/event.zig").Event;

const FunctionValidator = @import("Function.zig");

const types = wasm.types;
const sections = wasm.sections;
const indices = wasm.indices;

const Allocator = std.mem.Allocator;
const Section = sections.Section;

const log = std.log.scoped(.validator);

const MAX_PAGES = 65536;

pub const Config = struct {
    max_nesting_depth: u32 = 1024,
    max_locals: u32 = 50000,
    max_function_size: u32 = 128 * 1024, // 128KB
    max_params: u32 = 1024,
    max_returns: u32 = 1024,
};

config: Config,

pub fn init(config: Config) Validator {
    return .{ .config = config };
}

pub fn onEvent(validator: *const Validator, event: Event) !void {
    switch (event.payload) {
        .type_section => |section| try validator.validateTypeSection(section),
        .import_section => |section| try validator.validateImportSection(section, event.ctx),
        .func_section => |section| try validator.validateFuncSection(section, event.ctx),
        .table_section => |section| try validator.validateTableSection(section),
        .memory_section => |section| try validator.validateMemorySection(section),
        .global_section => |section| try validator.validateGlobalSection(section, event.ctx),
        .export_section => |section| try validator.validateExportSection(section, event.ctx),
        .start_section => |section| try validator.validateStartSection(section, event.ctx),
        //.element_section => |section| try validator.validateElementSection(section, event.ctx),
        .code_section => |section| try validator.validateCodeSection(section, event.ctx),
        //.data_section => |section| try validator.validateDataSection(section, event.ctx),
        else => {},
    }
}

fn validateTypeSection(self: *const Validator, section: Section(.type)) !void {
    var it = section.iter();
    while (try it.next()) |func_type| {
        if (func_type.params.len > self.config.max_params) return error.MaxFuncParamsReached;
        if (func_type.results.len > self.config.max_returns) return error.MaxFuncReturnsReached;
    }
}

fn validateImportSection(_: *const Validator, section: Section(.import), ctx: *const Context) !void {
    var it = section.iter();
    while (try it.next()) |import| {
        switch (import.desc) {
            .func => |type_idx| try validateTypeIdx(type_idx, ctx),
            .table => |table| try validateTable(table),
            .memory => |mem| try validateMemory(mem),
            .global => |global_type| {
                if (global_type.mut != .@"const") return error.ImportedGlobalMustBeImmutable;
            },
        }
    }
}

fn validateFuncSection(_: *const Validator, section: Section(.func), ctx: *const Context) !void {
    var it = section.iter();
    while (try it.next()) |type_idx| try validateTypeIdx(type_idx, ctx);
}

fn validateTypeIdx(func_idx: indices.Func, ctx: *const Context) !void {
    if (func_idx >= ctx.functypes.len) return error.FuncIndexOutOfBounds;
}

fn validateTableSection(_: *const Validator, section: Section(.table)) !void {
    var it = section.iter();
    while (try it.next()) |table| try validateTable(table);
}

fn validateTable(table: types.Table) !void {
    if (table.limits.max) |max| {
        if (table.limits.min > max) return error.InvalidTableLimits;
    }
}

fn validateMemorySection(_: *const Validator, section: Section(.memory)) !void {
    var it = section.iter();
    while (try it.next()) |mem| try validateMemory(mem);
}

fn validateMemory(mem: types.Memory) !void {
    if (mem.max) |max| {
        if (mem.min > max) return error.InvalidMemoryLimits;
    }
    if (mem.min > MAX_PAGES) return error.MemoryMinTooLarge;
    if (mem.max) |max| {
        if (max > MAX_PAGES) return error.MemoryMaxTooLarge;
    }
}

fn validateGlobalSection(_: *const Validator, section: Section(.global), ctx: *const Context) !void {
    var it = section.iter();
    while (try it.next()) |global| {
        try validateConstExpr(global.init_expr, global.ty.valtype, ctx);
    }
}

fn validateConstExpr(expr: types.Expr, expected_type: types.ValType, ctx: *const Context) !void {
    switch (expr) {
        .i32 => if (expected_type != .i32) return error.ConstExprTypeMismatch,
        .i64 => if (expected_type != .i64) return error.ConstExprTypeMismatch,
        .f32 => if (expected_type != .f32) return error.ConstExprTypeMismatch,
        .f64 => if (expected_type != .f64) return error.ConstExprTypeMismatch,
        .global => |global_idx| {
            if (global_idx >= ctx.globals.len) return error.GlobalIndexOutOfBounds;

            const global = ctx.globals[global_idx];
            if (global.ty.mut != .@"const") return error.ConstExprReferenceMutableGlobal;
            if (global.ty.valtype != expected_type) return error.ConstExprTypeMismatch;
        },
    }
}

fn validateExportSection(_: *const Validator, section: Section(.@"export"), ctx: *const Context) !void {
    var it = section.iter();

    var seen_names = std.StringHashMapUnmanaged(void){};
    defer seen_names.deinit(ctx.allocator);

    while (try it.next()) |exp| {
        if (seen_names.contains(exp.name)) return error.DuplicateExportName;
        try seen_names.put(ctx.allocator, exp.name, {});

        switch (exp.kind) {
            .func => |func_idx| {
                const total_funcs = ctx.imports.len + ctx.funcs.len;
                if (func_idx >= total_funcs) return error.ExportFuncIndexOutOfBounds;
            },
            .table => |table_idx| if (table_idx >= ctx.tables.len) return error.ExportTableIndexOutOfBounds,
            .memory => |mem_idx| if (mem_idx >= ctx.memories.len) return error.ExportMemoryIndexOutOfBounds,
            .global => |global_idx| if (global_idx >= ctx.globals.len) return error.ExportGlobalIndexOutOfBounds,
        }
    }
}

fn validateStartSection(_: *const Validator, section: Section(.start), ctx: *const Context) !void {
    if (section.func_idx >= ctx.funcs.len) return error.StartFuncIndexOutOfBounds;
}

fn validateElementSection(validator: *const Validator, section: Section(.elem), ctx: *const Context) !void {
    _ = validator;
    _ = section;
    _ = ctx;
    // TODO: Validate element segments
}

fn validateCodeSection(self: *const Validator, section: Section(.code), ctx: *const Context) !void {
    const config = FunctionValidator.Config{
        .max_function_size = self.config.max_function_size,
        .max_locals = self.config.max_locals,
        .max_nesting_depth = self.config.max_nesting_depth,
    };
    var it = section.iter();
    var i: u32 = 0;
    while (try it.next()) |body| {
        var func_validator = try FunctionValidator.init(
            body,
            i,
            ctx,
            config,
        );
        defer func_validator.deinit();
        try func_validator.validate();
        i += 1;
    }
}

fn validateDataSection(validator: *const Validator, section: Section(.data), ctx: *const Context) !void {
    _ = validator;
    _ = section;
    _ = ctx;
    // TODO: Validate data segments
}
