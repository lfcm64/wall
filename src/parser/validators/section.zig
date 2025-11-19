const std = @import("std");
const sections = @import("../../module/sections.zig");

const Store = @import("../store.zig").Store;

pub const Error = error{
    DuplicateSection,
    UnexpectedSection,
    FunctionIndexOutOfBounds,
    StartSectionIndexOutOfBounds,
    TableIndexOutOfBounds,
    MemoryIndexOutOfBounds,
    GlobalIndexOutOfBounds,
    ExportNameConflict,
    InvalidExportIndex,
    InvalidElementTableIndex,
    InvalidElementFunctionIndex,
    RemainingBytesInSection,
    InternalFailure,
};

pub const SectionValidatorState = enum(u8) {
    not_started,
    type,
    import,
    function,
    table,
    memory,
    global,
    @"export",
    start,
    element,
    code,
    data,
};

pub const SectionValidator = struct {
    state: SectionValidatorState = .not_started,

    pub fn validateTypeSection(self: *SectionValidator, store: *Store, section: sections.TypeSection) Error!void {
        try self.checkSectionOrder(.Type);
        self.state = .type;

        var it = section.iter();
        while (it.next() catch return Error.InternalFailure) |func_type| {
            store.append(.types, func_type) catch return Error.InternalFailure;
        }
        if (!it.isFinished()) return Error.RemainingBytesInSection;
    }

    pub fn validateImportSection(self: *SectionValidator, store: *Store, section: sections.ImportSection) Error!void {
        try self.checkSectionOrder(.Import);
        self.state = .import;

        var it = section.iter();
        while (it.next() catch return Error.InternalFailure) |import| {
            switch (import.kind) {
                .func => |func| store.append(.functions, func) catch return Error.InternalFailure,
                .table => |table| store.append(.tables, table) catch return Error.InternalFailure,
                .memory => |memory| store.append(.memories, memory) catch return Error.InternalFailure,
                .global => |global| store.append(.globals, global) catch return Error.InternalFailure,
            }
        }
        if (!it.isFinished()) return Error.RemainingBytesInSection;
    }

    pub fn validateFunctionSection(self: *SectionValidator, store: *Store, section: sections.FunctionSection) Error!void {
        try self.checkSectionOrder(.Function);
        self.state = .function;

        var it = section.iter();
        const type_count = store.count(.types);
        while (it.next() catch return Error.InternalFailure) |func_index| {
            if (func_index.val >= type_count) return Error.FunctionIndexOutOfBounds;
            store.append(.functions, func_index.val) catch return Error.InternalFailure;
        }
        if (!it.isFinished()) return Error.RemainingBytesInSection;
    }

    pub fn validateTableSection(self: *SectionValidator, store: *Store, section: sections.TableSection) Error!void {
        try self.checkSectionOrder(.Table);
        self.state = .table;

        var it = section.iter();
        while (it.next() catch return Error.InternalFailure) |table| {
            store.append(.tables, table) catch return Error.InternalFailure;
        }
        if (!it.isFinished()) return Error.RemainingBytesInSection;
    }

    pub fn validateMemorySection(self: *SectionValidator, store: *Store, section: sections.MemorySection) Error!void {
        try self.checkSectionOrder(.Memory);
        self.state = .memory;

        var it = section.iter();
        while (it.next() catch return Error.InternalFailure) |memory| {
            store.append(.memories, memory) catch return Error.InternalFailure;
        }
        if (!it.isFinished()) return Error.RemainingBytesInSection;
    }

    pub fn validateGlobalSection(self: *SectionValidator, store: *Store, section: sections.GlobalSection) Error!void {
        try self.checkSectionOrder(.Global);
        self.state = .global;

        var it = section.iter();
        while (it.next() catch return Error.InternalFailure) |global| {
            store.append(.globals, global.type) catch return Error.InternalFailure;
        }
        if (!it.isFinished()) return Error.RemainingBytesInSection;
    }

    pub fn validateExportSection(self: *SectionValidator, store: *Store, section: sections.ExportSection) Error!void {
        try self.checkSectionOrder(.Export);
        self.state = .@"export";

        var it = section.iter();
        while (it.next() catch return Error.InternalFailure) |exp| {
            store.append(.exports, exp) catch return Error.InternalFailure;
        }
        if (!it.isFinished()) return Error.RemainingBytesInSection;
    }

    pub fn validateStartSection(self: *SectionValidator, store: *Store, section: sections.StartSection) Error!void {
        try self.checkSectionOrder(.Start);
        self.state = .start;

        if (section.start >= store.count(.types)) return Error.StartSectionIndexOutOfBounds;
    }

    pub fn validateElementSection(self: *SectionValidator, store: *Store, section: sections.ElementSection) Error!void {
        try self.checkSectionOrder(.Element);
        self.state = .element;

        var it = section.iter();
        while (it.next() catch return Error.InternalFailure) |elem| {
            if (elem.table_idx >= store.count(.tables)) return Error.InvalidElementTableIndex;
        }
        if (!it.isFinished()) return Error.RemainingBytesInSection;
    }
    fn checkSectionOrder(self: *SectionValidator, section_type: sections.SectionType) Error!void {
        if (@intFromEnum(self.state) == @intFromEnum(section_type)) return Error.DuplicateSection;
        if (@intFromEnum(self.state) > @intFromEnum(section_type)) return Error.UnexpectedSection;
    }
};
