const std = @import("std");
const types = @import("../../module/types.zig");

const io = std.io;

pub const ElementStream = struct {
    reader: io.Reader,
    count: u32,

    index: u32 = 0,
    state: State = .new_elem,

    pub const State = union(enum) {
        new_elem,
        streaming: struct {
            current: types.element.ElementStart,
            index: u32 = 0,
        },
        end: void,
    };

    pub const Item = union(enum) {
        elem_start: types.element.ElementStart,
        index: types.VarU32,
    };

    pub fn next(self: *ElementStream) !?Item {
        switch (self.state) {
            .new_elem => return self.newElem(),
            .streaming => |*state| {
                if (state.current.index_count == state.index) {
                    return self.newElem();
                }
                const index = try types.VarU32.fromReader(&self.reader);
                state.index += 1;
                return Item{ .index = index };
            },
            .end => return null,
        }
    }

    fn newElem(self: *ElementStream) !?Item {
        if (self.index == self.count) {
            self.state = .end;
            return null;
        }
        const elem_start = try types.element.ElementStart.fromReader(&self.reader);
        self.state = .{ .streaming = .{ .current = elem_start } };

        return Item{ .elem_start = elem_start };
    }
};

pub const CodeStream = struct {
    reader: io.Reader,
    count: u32,

    index: u32 = 0,
    state: State = .new_func,

    pub const State = union(enum) {
        new_func,
        streaming: struct {
            current: types.function.FuncBodyStart,
            local_idx: u32 = 0,
            locals_size: u32 = 0,
        },
        end: void,
    };

    pub const Item = union(enum) {
        func_start: types.function.FuncBodyStart,
        local: types.function.FuncLocal,
        code: types.function.Code,
    };

    pub fn next(self: *CodeStream) !?Item {
        switch (self.state) {
            .new_func => return self.newFunc(),
            .streaming => |*state| {
                if (state.current.local_count == state.local_idx) {
                    const code_size = state.current.size - state.locals_size - 1;
                    const code = try self.reader.take(code_size);
                    self.state = .new_func;
                    self.index += 1;
                    return Item{ .code = @ptrCast(code) };
                }
                const initial_size = self.reader.seek;
                const local = try types.function.FuncLocal.fromReader(&self.reader);
                state.local_idx += 1;
                state.locals_size += @intCast(self.reader.seek - initial_size);
                return Item{ .local = local };
            },
            .end => return null,
        }
    }

    fn newFunc(self: *CodeStream) !?Item {
        if (self.index == self.count) {
            self.state = .end;
            return null;
        }
        const func_start = try types.function.FuncBodyStart.fromReader(&self.reader);
        self.state = .{ .streaming = .{ .current = func_start } };

        return Item{ .func_start = func_start };
    }
};
