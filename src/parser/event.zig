const wasm = @import("../wasm/wasm.zig");

const Context = @import("Context.zig");

const sections = wasm.sections;
const types = wasm.types;

const Section = sections.Section;

pub const Payload = union(enum) {
    module_header: types.Header,
    custom_section: Section(.custom),
    type_section: Section(.type),
    import_section: Section(.import),
    func_section: Section(.func),
    table_section: Section(.table),
    memory_section: Section(.memory),
    global_section: Section(.global),
    export_section: Section(.@"export"),
    start_section: Section(.start),
    element_section: Section(.elem),
    code_section: Section(.code),
    data_section: Section(.data),
};

pub const Event = struct {
    ctx: *const Context,
    payload: Payload,
};
