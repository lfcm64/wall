const Handler = @This();

const std = @import("std");
const wasm = @import("../wasm/wasm.zig");

const Parser = @import("Parser.zig");

const types = wasm.types;
const sections = wasm.sections;

const Event = Parser.Event;
const Section = sections.Section;

ptr: *anyopaque,
vtable: *const VTable,

pub const VTable = struct {
    onModuleHeader: ?*const fn (ptr: *anyopaque, types.Header) anyerror!void = null,
    onCustomSection: ?*const fn (ptr: *anyopaque, section: Section(.custom)) anyerror!void = null,
    onTypeSection: ?*const fn (ptr: *anyopaque, section: Section(.type)) anyerror!void = null,
    onImportSection: ?*const fn (ptr: *anyopaque, section: Section(.import)) anyerror!void = null,
    onFuncSection: ?*const fn (ptr: *anyopaque, section: Section(.func)) anyerror!void = null,
    onTableSection: ?*const fn (ptr: *anyopaque, section: Section(.table)) anyerror!void = null,
    onMemorySection: ?*const fn (ptr: *anyopaque, section: Section(.memory)) anyerror!void = null,
    onGlobalSection: ?*const fn (ptr: *anyopaque, section: Section(.global)) anyerror!void = null,
    onExportSection: ?*const fn (ptr: *anyopaque, section: Section(.@"export")) anyerror!void = null,
    onStartSection: ?*const fn (ptr: *anyopaque, section: Section(.start)) anyerror!void = null,
    onElementSection: ?*const fn (ptr: *anyopaque, section: Section(.elem)) anyerror!void = null,
    onCodeSection: ?*const fn (ptr: *anyopaque, section: Section(.code)) anyerror!void = null,
    onDataSection: ?*const fn (ptr: *anyopaque, section: Section(.data)) anyerror!void = null,
};

pub fn onEvent(self: *const Handler, event: Event) !void {
    switch (event) {
        inline else => |e, tag| {
            const field_name = "on" ++ @tagName(tag);
            if (@field(self.vtable, field_name)) |func| {
                try func(self.ptr, e);
            }
        },
    }
}
