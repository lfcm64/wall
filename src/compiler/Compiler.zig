const Compiler = @This();

const wasm = @import("../wasm/wasm.zig");

const CompilerContext = @import("Context.zig");
const ModuleContext = @import("../parser/Parser.zig");

const FunctionCompiler = @import("codegen/function.zig").FunctionCompiler;

const Event = @import("../parser/event.zig").Event;

const sections = wasm.sections;

const Section = sections.Section;

ctx: CompilerContext,

pub fn onEvent(self: *Compiler, event: Event) !void {
    switch (event.payload) {
        .code_section => |section| try self.compileFunctions(section, event.ctx),
        else => {},
    }
}

fn compileFunctions(self: *Compiler, section: Section(.code), mod_ctx: *const ModuleContext) !void {
    var it = section.iter();

    while (try it.next()) |body| {}
}
