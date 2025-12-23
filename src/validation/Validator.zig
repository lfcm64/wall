const Validator = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const State = @import("State.zig");

const Event = @import("../parser/event.zig").Event;

const FunctionValidator = @import("Function.zig");

const types = wasm.types;
const sections = wasm.sections;
const indices = wasm.indices;

const Allocator = std.mem.Allocator;
const Section = sections.Section;

const log = std.log.scoped(.validator);

state: State,

pub fn init(allocator: Allocator) Validator {
    return .{ .state = State.init(allocator) };
}

pub fn deinit(self: *Validator) void {
    self.state.deinit();
}

pub fn onEvent(validator: *Validator, event: Event) !void {
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
    return *const fn (state: *State, item: sections.SectionItem(section_type), idx: u32) anyerror!void;
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
        try validate_item(&self.state, item, i);
    }
}

fn validateTypeSection(self: *Validator, section: Section(.type)) !void {
    try self.validateEach(
        .type,
        section,
        struct {
            fn validate(state: *State, ty: types.FuncType, _: u32) !void {
                try state.addFuncType(ty);
            }
        }.validate,
    );
}

fn validateImportSection(self: *Validator, section: Section(.import)) !void {
    try self.validateEach(
        .import,
        section,
        struct {
            fn validate(state: *State, import: types.Import, _: u32) !void {
                switch (import.desc) {
                    .func => |type_idx| if (type_idx >= state.functypes.items.len) return error.FuncIndexOutOfBounds,
                    .table => |table| try verifyTable(table),
                    .memory => |mem| try verifyMemory(mem),
                    .global => |global_ty| if (global_ty.mut != .@"const") return error.ImportedGlobalMustBeImmutable,
                }
                try state.addImport(import);
            }
        }.validate,
    );
}

fn validateFuncSection(self: *Validator, section: Section(.func)) !void {
    try self.validateEach(
        .func,
        section,
        struct {
            fn validate(state: *State, type_idx: indices.Func, _: u32) !void {
                if (type_idx >= state.functypes.items.len) return error.FuncIndexOutOfBounds;
                try state.addFunc(type_idx);
            }
        }.validate,
    );
}

fn validateTableSection(self: *Validator, section: Section(.table)) !void {
    try self.validateEach(
        .table,
        section,
        struct {
            fn validate(state: *State, table: types.Table, _: u32) !void {
                try verifyTable(table);
                try state.addTable(table);
            }
        }.validate,
    );
}

fn validateMemorySection(self: *Validator, section: Section(.memory)) !void {
    try self.validateEach(
        .memory,
        section,
        struct {
            fn validate(state: *State, mem: types.Memory, _: u32) !void {
                try verifyMemory(mem);
                try state.addMemory(mem);
            }
        }.validate,
    );
}

fn validateGlobalSection(self: *Validator, section: Section(.global)) !void {
    try self.validateEach(
        .global,
        section,
        struct {
            fn validate(state: *State, global: types.Global, _: u32) !void {
                try validateConstExpr(state, global.init_expr, global.ty.valtype);
                try state.addGlobal(global);
            }
        }.validate,
    );
}

fn validateExportSection(self: *Validator, section: Section(.@"export")) !void {
    try self.validateEach(
        .@"export",
        section,
        struct {
            fn validate(state: *State, exp: types.Export, _: u32) !void {
                switch (exp.kind) {
                    .func => |func_idx| if (func_idx >= state.funcs.items.len) return error.ExportFuncIndexOutOfBounds,
                    .table => |table_idx| if (table_idx >= state.tables.items.len) return error.ExportTableIndexOutOfBounds,
                    .memory => |mem_idx| if (mem_idx >= state.memories.items.len) return error.ExportMemoryIndexOutOfBounds,
                    .global => |global_idx| if (global_idx >= state.globals.items.len) return error.ExportGlobalIndexOutOfBounds,
                }
                try state.addExport(exp);
            }
        }.validate,
    );
}

fn validateStartSection(self: *Validator, section: Section(.start)) !void {
    if (section.func_idx >= self.state.funcs.items.len) return error.StartFuncIndexOutOfBounds;
}

fn validateElementSection(self: *Validator, section: Section(.elem)) !void {
    try self.validateEach(
        .elem,
        section,
        struct {
            fn validate(state: *State, elem: types.Element, _: u32) !void {
                if (elem.table_idx >= state.tables.items.len) return error.TableIndexOutOfBounds;
                try validateConstExpr(state, elem.offset, .i32);

                const func_count = state.funcs.items.len;
                var it = elem.indices.iter();
                while (try it.next()) |func_idx| if (func_idx >= func_count) return error.ElementFuncIndexOutOfBounds;
            }
        }.validate,
    );
}

fn validateCodeSection(self: *Validator, section: Section(.code)) !void {
    try self.validateEach(
        .code,
        section,
        struct {
            fn validate(state: *State, body: types.FuncBody, idx: u32) !void {
                var func_validator = try FunctionValidator.init(body, idx, state);
                defer func_validator.deinit();

                try func_validator.validate();
            }
        }.validate,
    );
}

fn validateDataSection(self: *Validator, section: Section(.data)) !void {
    try self.validateEach(
        .data,
        section,
        struct {
            fn validate(state: *State, data: types.Segment, _: u32) !void {
                if (data.mem_idx >= state.memories.items.len) return error.MemoryIndexOutOfBounds;
                try validateConstExpr(state, data.offset, .i32);
            }
        }.validate,
    );
}

fn validateConstExpr(state: *State, expr: types.Expr, expected_type: types.ValType) !void {
    switch (expr) {
        .i32 => if (expected_type != .i32) return error.ConstExprTypeMismatch,
        .i64 => if (expected_type != .i64) return error.ConstExprTypeMismatch,
        .f32 => if (expected_type != .f32) return error.ConstExprTypeMismatch,
        .f64 => if (expected_type != .f64) return error.ConstExprTypeMismatch,
        .global => |global_idx| {
            if (global_idx >= state.imported_globals) return error.ConstExprReferencesNonImportedGlobal;

            const global = state.globals.items[global_idx];
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
