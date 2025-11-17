const std = @import("std");
const parser = @import("parser/parser.zig");
const printer = @import("parser/printer.zig");

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
        var p = parser.Parser.init(pipeline.source);
        var pr = printer.Printer.init(std.fs.File.stdout());

        while (try p.parseNext()) |parsed| {
            if (pipeline.config.print_ast) try pr.printParsed(parsed);
        }
    }
};
