const Expr = @This();

const std = @import("std");
const types = @import("types.zig");

const ValType = types.ValType;

value: Value,
global_idx: u32,

pub const Value = union(ValType) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
};
