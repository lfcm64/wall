const wasm = @import("../wasm/wasm.zig");

const Context = @import("Context.zig");

const sections = wasm.sections;
const types = wasm.types;

const Section = sections.Section;

pub const Payload = union(enum) {
    ModuleHeader: types.Header,
    CustomSection: Section(.custom),
    TypeSection: Section(.type),
    ImportSection: Section(.import),
    FuncSection: Section(.func),
    TableSection: Section(.table),
    MemorySection: Section(.memory),
    GlobalSection: Section(.global),
    ExportSection: Section(.@"export"),
    StartSection: Section(.start),
    ElementSection: Section(.elem),
    CodeSection: Section(.code),
    DataSection: Section(.data),
};

pub const Event = struct {
    ctx: *const Context,
    payload: Payload,
};
