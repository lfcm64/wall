const Compiler = @This();

const std = @import("std");
const wasm = @import("wasm");

const exports = @import("codegen/exports.zig");
const function = @import("codegen/function.zig");
const code = @import("codegen/code.zig");

const Context = @import("Context.zig");
const Event = @import("../parser/event.zig").Event;

const sections = wasm.sections;

const Allocator = std.mem.Allocator;
const Section = sections.Section;

ctx: Context,

pub fn init(allocator: Allocator) Compiler {
    return .{ .ctx = Context.init(allocator) };
}

pub fn deinit(self: *Compiler) void {
    self.ctx.deinit();
}

pub fn onEvent(self: *Compiler, event: Event) !void {
    switch (event) {
        .type_section => |section| try self.compileTypeSection(section),
        .func_section => |section| try self.compileFuncSection(section),
        .export_section => |section| try self.compileExportSection(section),
        .code_section => |section| try self.compileCodeSection(section),
        else => {},
    }
}

fn ItemCompiler(comptime section_type: sections.SectionType) type {
    return *const fn (ctx: *Context, item: sections.SectionItem(section_type), idx: u32) anyerror!void;
}

fn compileEach(
    self: *Compiler,
    comptime section_type: sections.SectionType,
    section: Section(section_type),
    compile_item: ItemCompiler(section_type),
) !void {
    var it = section.iter();
    var i: u32 = 0;
    while (try it.next()) |item| : (i += 1) {
        try compile_item(&self.ctx, item, i);
    }
}

fn compileTypeSection(self: *Compiler, section: Section(.type)) !void {
    try self.compileEach(
        .type,
        section,
        function.FunctionTypeCompiler.compile,
    );
}

fn compileFuncSection(self: *Compiler, section: Section(.func)) !void {
    try self.compileEach(
        .func,
        section,
        function.FunctionCompiler.compile,
    );
}

fn compileExportSection(self: *Compiler, section: Section(.@"export")) !void {
    try self.compileEach(
        .@"export",
        section,
        exports.ExportCompiler.compile,
    );
}

fn compileCodeSection(self: *Compiler, section: Section(.code)) !void {
    try self.compileEach(
        .code,
        section,
        code.CodeCompiler.compile,
    );
}
