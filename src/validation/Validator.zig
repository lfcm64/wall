const Validator = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const Context = @import("../parser/Context.zig");
const FunctionValidator = @import("Function.zig");
const Event = @import("../parser/event.zig").Event;

const sections = wasm.sections;

const Allocator = std.mem.Allocator;
const Section = sections.Section;

const log = std.log.scoped(.validation);

no_bug: u32 = 0,

pub fn onEvent(_: *const Validator, event: Event) !void {
    switch (event.payload) {
        .FuncSection => |section| try validateFuncSection(section, event.ctx),
        .CodeSection => |section| try validateCodeSection(section, event.ctx),
        else => {},
    }
}

pub fn validateFuncSection(section: Section(.func), ctx: *const Context) !void {
    var it = section.iter();
    while (try it.next()) |func| {
        if (func >= ctx.functypes.len) return error.FuncIndexOutOfBounds;
    }
}

pub fn validateCodeSection(section: Section(.code), ctx: *const Context) !void {
    var it = section.iter();
    var i: u32 = 0;
    while (try it.next()) |body| {
        var func_validator = try FunctionValidator.init(body, i, ctx);
        defer func_validator.deinit();
        try func_validator.validate();
        i += 1;
    }
}
