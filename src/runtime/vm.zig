const std = @import("std");

const Allocator = std.mem.Allocator;

pub const VmCtx = struct {
    allocator: Allocator,

    memory: []u8,

    pub fn init(allocator: Allocator) !VmCtx {
        return .{
            .allocator = allocator,
            .memory = try allocator.alloc(u8, 8),
        };
    }

    pub fn deinit(self: *VmCtx) void {
        self.allocator.free(self.memory);
    }
};

pub fn get_i8(ptr: *anyopaque, offset: u32) callconv(.c) i32 {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));

    if (offset + 1 > ctx.memory.len) @panic("memory access out of bounds");
    return @as(i32, ctx.memory[offset]);
}

pub fn set_i8(ptr: *anyopaque, offset: u32, value: i32) callconv(.c) void {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 1 > ctx.memory.len) @panic("memory access out of bounds");
    ctx.memory[offset] = @intCast(value & 0xFF);
}

pub fn get_i16(ptr: *anyopaque, offset: u32) callconv(.c) i32 {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 2 > ctx.memory.len) @panic("memory access out of bounds");
    return @as(i32, std.mem.readInt(u16, ctx.memory[offset..][0..2], .little));
}

pub fn set_i16(ptr: *anyopaque, offset: u32, value: i32) callconv(.c) void {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 2 > ctx.memory.len) @panic("memory access out of bounds");
    std.mem.writeInt(u16, ctx.memory[offset..][0..2], @intCast(value & 0xFFFF), .little);
}

pub fn get_i32(ptr: *anyopaque, offset: u32) callconv(.c) i32 {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 4 > ctx.memory.len) @panic("memory access out of bounds");
    return @bitCast(std.mem.readInt(u32, ctx.memory[offset..][0..4], .little));
}

pub fn set_i32(ptr: *anyopaque, offset: u32, value: i32) callconv(.c) void {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 4 > ctx.memory.len) @panic("memory access out of bounds");
    std.mem.writeInt(u32, ctx.memory[offset..][0..4], @bitCast(value), .little);
}

pub fn get_i64(ptr: *anyopaque, offset: u32) callconv(.c) i64 {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 8 > ctx.memory.len) @panic("memory access out of bounds");
    return @bitCast(std.mem.readInt(u64, ctx.memory[offset..][0..8], .little));
}

pub fn set_i64(ptr: *anyopaque, offset: u32, value: i64) callconv(.c) void {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 8 > ctx.memory.len) @panic("memory access out of bounds");
    std.mem.writeInt(u64, ctx.memory[offset..][0..8], @bitCast(value), .little);
}

pub fn get_f32(ptr: *anyopaque, offset: u32) callconv(.c) f32 {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 4 > ctx.memory.len) @panic("memory access out of bounds");
    const bits = std.mem.readInt(u32, ctx.memory[offset..][0..4], .little);
    return @bitCast(bits);
}

pub fn set_f32(ptr: *anyopaque, offset: u32, value: f32) callconv(.c) void {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 4 > ctx.memory.len) @panic("memory access out of bounds");
    std.mem.writeInt(u32, ctx.memory[offset..][0..4], @bitCast(value), .little);
}

pub fn get_f64(ptr: *anyopaque, offset: u32) callconv(.c) f64 {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 8 > ctx.memory.len) @panic("memory access out of bounds");
    const bits = std.mem.readInt(u64, ctx.memory[offset..][0..8], .little);
    return @bitCast(bits);
}

pub fn set_f64(ptr: *anyopaque, offset: u32, value: f64) callconv(.c) void {
    const ctx: *VmCtx = @ptrCast(@alignCast(ptr));
    if (offset + 8 > ctx.memory.len) @panic("memory access out of bounds");
    std.mem.writeInt(u64, ctx.memory[offset..][0..8], @bitCast(value), .little);
}
