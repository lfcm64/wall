const std = @import("std");
const Parser = @import("Parser.zig");
const Validator = @import("Validator.zig");
const Printer = @import("Printer.zig");

const Allocator = std.mem.Allocator;

pub const PipelineConfig = struct {
    print_ast: bool = false,
};

pub const FrontendPipeline = struct {
    allocator: Allocator,
    source: []const u8,
    config: PipelineConfig,

    pub fn init(allocator: Allocator, source: []const u8, config: PipelineConfig) FrontendPipeline {
        return .{
            .allocator = allocator,
            .source = source,
            .config = config,
        };
    }

    pub fn run(pipeline: *FrontendPipeline) !void {
        var p = Parser.init(pipeline.source);
        var v = Validator.init(pipeline.allocator);
        defer v.deinit();

        var pr = Printer.init(std.fs.File.stdout());

        while (try p.parseNext()) |payload| {
            try v.validate(payload);
            if (pipeline.config.print_ast) try pr.printPayload(payload);
        }
    }
};
