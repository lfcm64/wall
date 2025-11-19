const std = @import("std");
const types = @import("../module/types.zig");
//const sections = @import("sections/sections.zig");
const parser = @import("parser.zig");

const sections = types.sections;

const Payload = parser.Payload;

const io = std.io;
const File = std.fs.File;

pub const Printer = struct {
    file: File,

    pub fn init(file: File) Printer {
        return .{ .file = file };
    }

    pub fn printPayload(printer: *Printer, payload: Payload) !void {
        switch (payload) {
            .module_header => |header| try printer.printModuleHeader(header),
            .type_section => |section| try printer.printTypeSection(section),
            .import_section => |section| try printer.printImportSection(section),
            .function_section => |section| try printer.printFunctionSection(section),
            .table_section => |section| try printer.printTableSection(section),
            .memory_section => |section| try printer.printMemorySection(section),
            .global_section => |section| try printer.printGlobalSection(section),
            .export_section => |section| try printer.printExportSection(section),
            .start_section => |section| try printer.printStartSection(section),
            .element_section => |section| try printer.printElementSection(section),
            .code_section => |section| try printer.printCodeSection(section),
            else => {},
        }
    }

    fn printModuleHeader(printer: *Printer, header: types.module.ModuleHeader) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Module Header\n", .{});
        try writer.print("     {}\n", .{header});
        try writer.flush();
    }

    fn printTypeSection(printer: *Printer, section: sections.TypeSection) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Type Section\n", .{});
        try writer.flush();

        var it = section.iter();
        while (try it.next()) |func_type| {
            try writer.print("     Type {}: {}\n", .{ it.index - 1, func_type });
            try writer.flush();
        }
    }

    fn printImportSection(printer: *Printer, section: sections.ImportSection) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Import Section\n", .{});
        try writer.flush();

        var it = section.iter();
        while (try it.next()) |imp| {
            try writer.print("     Import {}: {}\n", .{ it.index - 1, imp });
            try writer.flush();
        }
    }

    fn printFunctionSection(printer: *Printer, section: sections.FunctionSection) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Function Section\n", .{});
        try writer.flush();

        var it = section.iter();
        while (try it.next()) |func_idx| {
            try writer.print("     Function {}: {}\n", .{ it.index - 1, func_idx });
            try writer.flush();
        }
    }

    fn printTableSection(printer: *Printer, section: sections.TableSection) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Table Section\n", .{});
        try writer.flush();

        var it = section.iter();
        while (try it.next()) |table| {
            try writer.print("     Table {}: {}\n", .{ it.index - 1, table });
            try writer.flush();
        }
    }

    fn printMemorySection(printer: *Printer, section: sections.MemorySection) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Memory Section\n", .{});
        try writer.flush();

        var it = section.iter();
        while (try it.next()) |mem| {
            try writer.print("     Memory {}: {}\n", .{ it.index - 1, mem });
            try writer.flush();
        }
    }

    fn printGlobalSection(printer: *Printer, section: sections.GlobalSection) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Global Section\n", .{});
        try writer.flush();

        var it = section.iter();
        while (try it.next()) |global| {
            try writer.print("     Global {}: {}\n", .{ it.index - 1, global });
            try writer.flush();
        }
    }

    fn printExportSection(printer: *Printer, section: sections.ExportSection) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Export Section\n", .{});
        try writer.flush();

        var it = section.iter();
        while (try it.next()) |exp| {
            try writer.print("     Export {}: {}\n", .{ it.index - 1, exp });
            try writer.flush();
        }
    }

    fn printStartSection(printer: *Printer, section: sections.StartSection) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Start Section\n", .{});
        try writer.flush();

        try writer.print("     Start Function: {}\n", .{section});
        try writer.flush();
    }

    pub fn printCodeSection(printer: *Printer, section: sections.CodeSection) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Code Section: {} functions\n", .{section.count});
        try writer.flush();

        var it = section.iter();
        while (try it.next()) |func| {
            try writer.print(
                "     FuncBody {}: size: {}, ",
                .{ it.index - 1, func.size },
            );

            var local_it = func.locals.iter();
            while (try local_it.next()) |local| {
                try writer.print("locals: [", .{});
                try writer.print(
                    "{}, ",
                    .{local},
                );
                try writer.print("], ", .{});
            }
            try writer.print(
                "code: {any}\n",
                .{func.code},
            );
            try writer.flush();
        }
    }

    pub fn printElementSection(printer: *Printer, section: sections.ElementSection) !void {
        var buf: [1024]u8 = undefined;
        var file_writer = printer.file.writer(&buf);
        const writer = &file_writer.interface;

        try writer.print("Element Section: {} elements\n", .{section.count});
        try writer.flush();

        var it = section.iter();
        while (try it.next()) |elem| {
            try writer.print(
                "     Element {}: table_idx: {}, offset: {}, ",
                .{ it.index - 1, elem.table_idx, elem.offset },
            );

            var indices_it = elem.indices.iter();
            while (try indices_it.next()) |index| {
                try writer.print("indices: [", .{});
                try writer.print(
                    "{}, ",
                    .{index},
                );
                try writer.print("]\n", .{});
            }
            try writer.flush();
        }
    }
};
