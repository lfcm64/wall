const State = @This();

const std = @import("std");
const llvm = @import("llvm");
const wasm = @import("../wasm/wasm.zig");

const types = llvm.types;

const Allocator = std.mem.Allocator;

const BasicBlock = types.LLVMBasicBlockRef;
const Value = types.LLVMValueRef;
const BlockType = wasm.instr.BlockType;

stack: std.ArrayList(Value) = .{},
control_stack: std.ArrayList(ControlFrame) = .{},
reachable: bool = true,

pub const ControlFrame = union(enum) {
    block: struct {
        next: BasicBlock,
        phi: ?Value,
    },

    loop: struct {
        header: BasicBlock,
        next: BasicBlock,
        loop_phi: ?Value,
        phi: ?Value,
    },

    if_else: struct {
        then: BasicBlock,
        @"else": BasicBlock,
        has_else: bool = false,
        next: BasicBlock,
        phi: ?Value,
    },

    pub fn next(self: *const ControlFrame) BasicBlock {
        return switch (self.*) {
            .block => |block| block.next,
            .loop => |loop| loop.next,
            .if_else => |ie| ie.next,
        };
    }

    pub fn brDest(self: *const ControlFrame) BasicBlock {
        return switch (self.*) {
            .block => |block| block.next,
            .loop => |loop| loop.header,
            .if_else => |ie| ie.next,
        };
    }

    pub fn phi(self: *const ControlFrame) ?Value {
        return switch (self.*) {
            .block => |block| block.phi,
            .loop => |loop| loop.loop_phi,
            .if_else => |ie| ie.phi,
        };
    }
};

pub fn deinit(self: *State, allocator: Allocator) void {
    self.stack.deinit(allocator);
    self.control_stack.deinit(allocator);
}

pub fn push(self: *State, allocator: Allocator, value: Value) !void {
    try self.stack.append(allocator, value);
}

pub fn pop(self: *State) ?Value {
    return self.stack.pop();
}

pub fn peek(self: *const State) ?Value {
    if (self.stack.items.len == 0) return null;
    return self.stack.items[self.stack.items.len - 1];
}

pub fn pushFrame(self: *State, allocator: std.mem.Allocator, frame: ControlFrame) !void {
    try self.control_stack.append(allocator, frame);
}

pub fn popFrame(self: *State) ?ControlFrame {
    return self.control_stack.pop();
}

pub fn frameAtDepth(self: *State, depth: usize) ?*ControlFrame {
    if (depth >= self.control_stack.items.len) return null;
    return &self.control_stack.items[self.control_stack.items.len - 1 - depth];
}

pub fn currentFrame(self: *State) ?*ControlFrame {
    if (self.control_stack.items.len == 0) return null;
    return &self.control_stack.items[self.control_stack.items.len - 1];
}
