const Validator = @This();

const std = @import("std");
const module = @import("../module/module.zig");
const types = @import("../module/types.zig");

const Parser = @import("Parser.zig");
const Context = @import("Context.zig");
const OperandValidator = @import("Operand.zig");

const Section = module.Section;
const Allocator = std.mem.Allocator;
const Payload = Parser.Payload;

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
        .export_section => |section| try self.validateExportSection(section),
        .start_section => |section| try self.validateStartSection(section),
        .element_section => |section| try self.validateElementSection(section),
        .code_section => |section| try self.validateCodeSection(section),
        .data_section => |section| try self.validateDataSection(section),
        else => {},
    };
}

fn validateTypeSection(self: *Validator, section: Section(.type)) !void {
    const visitor = Section(.type).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, functype: types.Function.Type, _: u32) anyerror!void {
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
            fn visit(ctx: *anyopaque, func: types.primitives.VarU32, _: u32) anyerror!void {
                const v: *Validator = @ptrCast(@alignCast(ctx));
                if (v.context.funcTypeCount() <= func.val) return error.FuncIndexOutOfBounds;

                try v.context.addFunc(v.allocator, func.val);
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
    if (self.context.funcCount() <= section.val) return error.FuncIndexOutOfBounds;
}

fn validateElementSection(self: *Validator, section: Section(.elem)) !void {
    _ = self;
    _ = section;
}

fn validateCodeSection(self: *Validator, section: Section(.code)) !void {
    const visitor = Section(.code).Visitor{
        .ptr = self,
        .visit = struct {
            fn visit(ctx: *anyopaque, body: types.Function, idx: u32) anyerror!void {
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
    _ = self;
    _ = section;
}
