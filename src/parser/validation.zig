const std = @import("std");
const parser = @import("parser.zig");
const types = @import("../module/types.zig");
const sv = @import("validators/section.zig");
const fv = @import("validators/function.zig");

const sections = types.sections;

const Store = @import("store.zig").Store;

const Allocator = std.mem.Allocator;

const Payload = parser.Payload;

const SectionValidatorError = sv.Error;

pub const Error = error{
    BadMagicNumber,
    BadVersionNumber,
    UnexpectedSection,
};

pub const State = enum {
    header,
    sections,
    functions,
};

pub const Validator = struct {
    allocator: Allocator,

    store: Store,

    section_validator: sv.SectionValidator = .{},
    //func_validator: fv.FunctionValidator,

    state: State = .header,

    pub fn init(allocator: Allocator) Validator {
        return .{
            .allocator = allocator,
            //.func_validator = fv.FunctionValidator.init(allocator),
            .store = Store.init(allocator),
        };
    }

    pub fn deinit(self: *Validator) void {
        self.store.deinit(self.allocator);
    }

    pub fn validatePayload(self: *Validator, payload: Payload) (Error || SectionValidatorError)!void {
        switch (payload) {
            .module_header => |header| try self.validateModuleHeader(header),
            .code_section => |_| {},
            else => return self.validateSectionPayload(payload),
        }
    }

    fn validateSectionPayload(self: *Validator, payload: Payload) (Error || SectionValidatorError)!void {
        if (self.state == .functions) return Error.UnexpectedSection;
        switch (payload) {
            .type_section => |section| try self.section_validator.validateTypeSection(
                &self.store,
                section,
            ),
            .import_section => |section| try self.section_validator.validateImportSection(
                &self.store,
                section,
            ),
            .function_section => |section| try self.section_validator.validateFunctionSection(
                &self.store,
                section,
            ),
            .table_section => |section| try self.section_validator.validateTableSection(
                &self.store,
                section,
            ),
            .memory_section => |section| try self.section_validator.validateMemorySection(
                &self.store,
                section,
            ),
            .global_section => |section| try self.section_validator.validateGlobalSection(
                &self.store,
                section,
            ),
            .export_section => |section| try self.section_validator.validateExportSection(
                &self.store,
                section,
            ),
            .start_section => |section| try self.section_validator.validateStartSection(
                &self.store,
                section,
            ),
            .element_section => |section| try self.section_validator.validateElementSection(
                &self.store,
                section,
            ),
            else => unreachable,
        }
    }

    fn validateModuleHeader(_: *Validator, header: types.module.ModuleHeader) Error!void {
        if (header.magic != 1836278016) return Error.BadMagicNumber;
        if (header.version != 1) return Error.BadVersionNumber;
    }
};
