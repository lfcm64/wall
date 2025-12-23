const std = @import("std");

const llvm = @import("llvm");
const wasm = @import("../../wasm/wasm.zig");
const conv = @import("conv.zig");

const Intrinsic = @import("../intrinsics.zig").Intrinsic;
const Context = @import("../Context.zig");

const instr = wasm.instr;
const types = llvm.types;
const core = llvm.core;

const Allocator = std.mem.Allocator;

pub const BlockInfo = struct {
    llvm_func: types.LLVMValueRef,

    locals: []types.LLVMTypeRef,
    code: []const u8,
};

pub const BlockContext = struct {
    kind: BlockKind,
    continue_block: types.LLVMBasicBlockRef,
    break_block: types.LLVMBasicBlockRef,
    result_types: []const wasm.types.ValType,
    stack_height: usize,
    else_block: ?types.LLVMBasicBlockRef,
};

pub const BlockKind = enum {
    block,
    loop,
    if_then,
    if_else,
};

pub const BlockCompiler = struct {
    pub fn compile(ctx: *Context, block_info: BlockInfo) !void {
        const allocator = ctx.allocator;

        var stack: std.ArrayList(types.LLVMValueRef) = .{};
        errdefer stack.deinit(allocator);

        var block_stack: std.ArrayList(BlockContext) = .{};
        errdefer block_stack.deinit(allocator);

        const locals = block_info.locals;

        const entry = core.LLVMAppendBasicBlock(ctx.llvm_func, "entry");
        const builder = core.LLVMCreateBuilder();

        core.LLVMPositionBuilderAtEnd(builder, entry);

        var it = instr.Iterator.init(ctx.func_body.code);

        while (try it.next()) |in| {
            switch (in) {
                //call
                .@"if" => |block_type| {
                    const cond = stack.pop() orelse return error.StackUnderflow;

                    const then_block = core.LLVMAppendBasicBlockInContext(
                        ctx.llvm_context,
                        block_info.llvm_func,
                        "if.then",
                    );
                    const else_block = core.LLVMAppendBasicBlockInContext(
                        ctx.llvm_context,
                        block_info.llvm_func,
                        "if.else",
                    );
                    const merge_block = core.LLVMAppendBasicBlockInContext(
                        ctx.llvm_context,
                        block_info.llvm_func,
                        "if.end",
                    );

                    const bool_cond = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntNE,
                        cond,
                        core.LLVMConstInt(core.LLVMTypeOf(cond), 0, 0),
                        "",
                    );

                    _ = core.LLVMBuildCondBr(builder, bool_cond, then_block, else_block);
                    core.LLVMPositionBuilderAtEnd(builder, then_block);

                    const result_types = switch (block_type) {
                        .empty => &[_]wasm.types.ValType{},
                        .valtype => |vt| &[_]wasm.types.ValType{vt},
                    };

                    try block_stack.append(allocator, .{
                        .kind = .if_then,
                        .continue_block = merge_block,
                        .break_block = merge_block,
                        .result_types = result_types,
                        .stack_height = stack.items.len,
                        .else_block = else_block,
                    });
                },

                .end => {
                    const block = block_stack.pop().?;

                    if (block.kind == .if_then and block.else_block != null) {
                        _ = core.LLVMBuildBr(builder, block.continue_block);

                        core.LLVMPositionBuilderAtEnd(builder, block.else_block.?);
                        _ = core.LLVMBuildBr(builder, block.continue_block);

                        core.LLVMPositionBuilderAtEnd(builder, block.continue_block);
                    } else {
                        _ = core.LLVMBuildBr(builder, block.continue_block);
                        core.LLVMPositionBuilderAtEnd(builder, block.continue_block);
                    }
                },

                .@"return" => {
                    const arg = stack.pop().?;
                    _ = core.LLVMBuildRet(builder, arg);
                },

                .@"local.get" => |idx| {
                    const local = locals[idx + 1];
                    try stack.append(allocator, local);
                },

                .@"i32.const" => |value| {
                    const const_val = core.LLVMConstInt(
                        core.LLVMInt32Type(),
                        @intCast(value),
                        0,
                    );
                    try stack.append(allocator, const_val);
                },

                .@"i64.const" => |value| {
                    const const_val = core.LLVMConstInt(
                        core.LLVMInt64Type(),
                        @intCast(value),
                        0,
                    );
                    try stack.append(allocator, const_val);
                },

                .@"f32.const" => |value| {
                    const const_val = core.LLVMConstReal(core.LLVMFloatType(), value);
                    try stack.append(allocator, const_val);
                },

                .@"f64.const" => |value| {
                    const const_val = core.LLVMConstReal(core.LLVMDoubleType(), value);
                    try stack.append(allocator, const_val);
                },

                .@"i32.eqz", .@"i64.eqz" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntEQ,
                        val,
                        core.LLVMConstInt(core.LLVMInt32Type(), 0, 0),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.eq", .@"i64.eq" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntEQ,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.ne", .@"i64.ne" => { // not equal
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntNE,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.lt_s", .@"i64.lt_s" => { // less than (signed)
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntSLT,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.lt_u", .@"i64.lt_u" => { // less than (unsigned)
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntULT,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.gt_s", .@"i64.gt_s" => { // greater than (signed)
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntSGT,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.gt_u", .@"i64.gt_u" => { // greater than (unsigned)
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntUGT,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.le_s", .@"i64.le_s" => { // less than or equal (signed)
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntSLE,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.le_u", .@"i64.le_u" => { // less than or equal (unsigned)
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntULE,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.ge_s", .@"i64.ge_s" => { // greater than or equal (signed)
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntSGE,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.ge_u", .@"i64.ge_u" => { // greater than or equal (unsigned)
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntUGE,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.eq", .@"f64.eq" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealOEQ,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.ne", .@"f64.ne" => { // not equal
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealONE,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.lt", .@"f64.lt" => { // less than
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealOLT,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.gt", .@"f64.gt" => { // greather than
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealOGT,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.le", .@"f64.le" => { // greather than or equal
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealOLE,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.ge", .@"f64.ge" => { // greather than or equal
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealOGE,
                        arg1,
                        arg2,
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.clz" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{
                        val,
                        core.LLVMConstInt(core.LLVMInt1Type(), 0, 0),
                    };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ctlz.i32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.ctz" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{
                        val,
                        core.LLVMConstInt(core.LLVMInt1Type(), 0, 0),
                    };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.cttz.i32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.popcnt" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ctpop.i32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"i64.clz" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{
                        val,
                        core.LLVMConstInt(core.LLVMInt1Type(), 0, 0),
                    };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ctlz.i64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"i64.ctz" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{
                        val,
                        core.LLVMConstInt(core.LLVMInt1Type(), 0, 0),
                    };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.cttz.i64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"i64.popcnt" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ctpop.i64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.add", .@"i64.add" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const sum = core.LLVMBuildAdd(builder, arg1, arg2, "");
                    try stack.append(allocator, sum);
                },

                .@"i32.sub", .@"i64.sub" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const sum = core.LLVMBuildSub(builder, arg1, arg2, "");
                    try stack.append(allocator, sum);
                },

                .@"i32.mul", .@"i64.mul" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const sum = core.LLVMBuildMul(builder, arg1, arg2, "");
                    try stack.append(allocator, sum);
                },

                .@"i32.div_s", .@"i64.div_s" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const sum = core.LLVMBuildSDiv(builder, arg1, arg2, "");
                    try stack.append(allocator, sum);
                },

                .@"i32.div_u", .@"i64.div_u" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const sum = core.LLVMBuildUDiv(builder, arg1, arg2, "");
                    try stack.append(allocator, sum);
                },

                .@"i32.rem_s", .@"i64.rem_s" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildSRem(builder, arg1, arg2, "");
                    try stack.append(allocator, result);
                },

                .@"i32.rem_u", .@"i64.rem_u" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildURem(builder, arg1, arg2, "");
                    try stack.append(allocator, result);
                },

                .@"i32.and", .@"i64.and" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildAnd(builder, arg1, arg2, "");
                    try stack.append(allocator, result);
                },

                .@"i32.or", .@"i64.or" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildOr(builder, arg1, arg2, "");
                    try stack.append(allocator, result);
                },

                .@"i32.xor", .@"i64.xor" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildXor(builder, arg1, arg2, "");
                    try stack.append(allocator, result);
                },

                .@"i32.shl", .@"i64.shl" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildShl(builder, arg1, arg2, "");
                    try stack.append(allocator, result);
                },

                .@"i32.shr_s", .@"i64.shr_s" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildAShr(builder, arg1, arg2, "");
                    try stack.append(allocator, result);
                },

                .@"i32.shr_u", .@"i64.shr_u" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const result = core.LLVMBuildLShr(builder, arg1, arg2, "");
                    try stack.append(allocator, result);
                },

                .@"i32.rotl" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fshl.i32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.rotr" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fshr.i32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"i64.rotl" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fshl.i64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"i64.rotr" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fshr.i64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.abs" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fabs.f32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.neg", .@"f64.neg" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFNeg(builder, val, "");
                    try stack.append(allocator, result);
                },

                .@"f32.ceil" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ceil.f32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.floor" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.floor.f32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.trunc" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.trunc.f32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.nearest" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.nearbyint.f32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.sqrt" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.sqrt.f32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.min" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.minnum.f32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.max" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.maxnum.f32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.copysign" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.copysign.f32",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.abs" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fabs.f64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.ceil" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ceil.f64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.floor" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.floor.f64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.trunc" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.trunc.f64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.nearest" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.nearbyint.f64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.sqrt" => {
                    const val = stack.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.sqrt.f64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.add", .@"f64.add" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const sum = core.LLVMBuildFAdd(builder, arg1, arg2, "");
                    try stack.append(allocator, sum);
                },

                .@"f32.sub", .@"f64.sub" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const sum = core.LLVMBuildFSub(builder, arg1, arg2, "");
                    try stack.append(allocator, sum);
                },

                .@"f32.mul", .@"f64.mul" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const sum = core.LLVMBuildFMul(builder, arg1, arg2, "");
                    try stack.append(allocator, sum);
                },

                .@"f32.div", .@"f64.div" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    const sum = core.LLVMBuildFDiv(builder, arg1, arg2, "");
                    try stack.append(allocator, sum);
                },

                .@"f64.min" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.minnum.f64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.max" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.maxnum.f64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.copysign" => {
                    const arg2 = stack.pop().?;
                    const arg1 = stack.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.copysign.f64",
                        &args,
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.wrap_i64" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildTrunc(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.trunc_f32_s" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFPToSI(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.trunc_f32_u" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFPToUI(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.trunc_f64_s" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFPToSI(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.trunc_f64_u" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFPToUI(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i64.extend_i32_s" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildSExt(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i64.extend_i32_u" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildZExt(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i64.trunc_f32_s" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFPToSI(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },
                .@"i64.trunc_f32_u" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFPToUI(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },
                .@"i64.trunc_f64_s" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFPToSI(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },
                .@"i64.trunc_f64_u" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFPToUI(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.convert_i32_s" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildSIToFP(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.convert_i32_u" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildUIToFP(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.convert_i64_s" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildSIToFP(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.convert_i64_u" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildUIToFP(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.demote_f64" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFPTrunc(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.convert_i32_s" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildSIToFP(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.convert_i32_u" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildUIToFP(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.convert_i64_s" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildSIToFP(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.convert_i64_u" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildUIToFP(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.promote_f32" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildFPExt(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i32.reinterpret_f32" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildBitCast(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f32.reinterpret_i32" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildBitCast(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"i64.reinterpret_f64" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildBitCast(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                .@"f64.reinterpret_i64" => {
                    const val = stack.pop().?;
                    const result = core.LLVMBuildBitCast(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try stack.append(allocator, result);
                },

                else => unreachable,
            }
        }
    }
};

fn buildIntrinsicCall(
    builder: types.LLVMBuilderRef,
    intrinsic: Intrinsic,
    args: []types.LLVMValueRef,
) types.LLVMValueRef {
    return core.LLVMBuildCall2(
        builder,
        intrinsic.ty,
        intrinsic.func,
        @ptrCast(args),
        @intCast(args.len),
        "",
    );
}
