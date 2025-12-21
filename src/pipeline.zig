const std = @import("std");

const Parser = @import("parser/Parser.zig");

pub fn Pipeline(comptime EventHandlerTypes: []const type) type {
    return struct {
        const Self = @This();

        handlers: std.meta.Tuple(EventHandlerTypes),

        pub fn init(handlers: std.meta.Tuple(EventHandlerTypes)) Self {
            return .{ .handlers = handlers };
        }

        pub fn run(self: *Self, parser: *Parser) !void {
            while (try parser.parseNext()) |event| {
                inline for (self.handlers) |handler| try handler.onEvent(event);
            }
        }
    };
}
