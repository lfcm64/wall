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

pub const FunctionCtx = struct {
    ty: types.LLVMTypeRef,
    func: types.LLVMValueRef,
};

pub const CodeCompiler = struct {
    pub fn compile(ctx: *Context, body: wasm.types.FuncBody, idx: u32) !void {
        const func = ctx.llvm_funcs.items[ctx.imported_funcs + idx];
        const func_type = core.LLVMGlobalGetValueType(func);

        const entry = core.LLVMAppendBasicBlockInContext(ctx.llvm_context, func, "");

        const builder = core.LLVMCreateBuilder();
        defer core.LLVMDisposeBuilder(builder);

        core.LLVMPositionBuilderAtEnd(builder, entry);

        const locals = try createLocals(ctx, body, builder);
        defer ctx.allocator.free(locals);

        var stack: std.ArrayList(types.LLVMValueRef) = .{};
        defer stack.deinit(ctx.allocator);

        const func_ctx = FunctionCtx{ .ty = func_type, .func = func };

        var instrs = instr.Iterator.init(body.code);

        _ = try BlockCompiler.compile(
            ctx,
            func_ctx,
            locals,
            &stack,
            &instrs,
            entry,
            builder,
        );
    }

    fn createLocals(ctx: *Context, body: wasm.types.FuncBody, builder: types.LLVMBuilderRef) ![]types.LLVMValueRef {
        var locals: std.ArrayList(types.LLVMValueRef) = .{};
        errdefer locals.deinit(ctx.allocator);

        var it = body.locals.iter();

        while (try it.next()) |local| {
            const llvm_type = conv.wasmToLLVMTypeInContext(local.valtype, ctx.llvm_context);
            const zero = core.LLVMConstNull(llvm_type);

            for (0..local.count) |_| {
                const loc = core.LLVMBuildAlloca(builder, llvm_type, "");
                _ = core.LLVMBuildStore(builder, zero, loc);
                try locals.append(ctx.allocator, loc);
            }
        }
        return locals.toOwnedSlice(ctx.allocator);
    }
};

pub const BlockExit = enum {
    end,
    @"else",
    @"unreachable",
    branch,
};

pub const BlockResult = struct {
    exit: BlockExit,
    terminated: bool,
};

pub const BlockCompiler = struct {
    pub fn compile(
        ctx: *Context,
        func_ctx: FunctionCtx,
        locals: []types.LLVMValueRef,
        stack: *std.ArrayList(types.LLVMValueRef),
        instrs: *instr.Iterator,
        current_bb: types.LLVMBasicBlockRef,
        builder: types.LLVMBuilderRef,
    ) !BlockResult {
        const llvm_ctx = ctx.llvm_context;

        const func = func_ctx.func;
        const allocator = ctx.allocator;

        core.LLVMPositionBuilderAtEnd(builder, current_bb);

        const return_type = core.LLVMGetReturnType(func_ctx.ty);
        const has_return = core.LLVMGetTypeKind(return_type) != types.LLVMTypeKind.LLVMVoidTypeKind;

        var block_terminated = false;

        while (try instrs.next()) |in| {
            switch (in) {
                .call => |idx| {
                    const callee = ctx.llvm_funcs.items[idx];
                    const callee_type = core.LLVMGlobalGetValueType(callee);

                    const param_count = core.LLVMCountParams(callee);

                    const args = try allocator.alloc(types.LLVMValueRef, param_count);
                    defer allocator.free(args);

                    args[0] = core.LLVMGetParam(func, 0);

                    var i: usize = param_count - 1;
                    while (i > 0) : (i -= 1) {
                        args[i] = stack.pop() orelse return error.StackUnderflow;
                    }

                    const call_result = core.LLVMBuildCall2(
                        builder,
                        callee_type,
                        callee,
                        args.ptr,
                        @intCast(param_count),
                        "",
                    );

                    const callee_return_type = core.LLVMGetReturnType(callee_type);
                    if (core.LLVMGetTypeKind(callee_return_type) != types.LLVMTypeKind.LLVMVoidTypeKind) {
                        try stack.append(allocator, call_result);
                    }
                },

                .@"if" => |_| {
                    const cond = stack.pop().?;

                    const then_bb = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "");
                    const else_bb = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "");
                    const merge_bb = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "");

                    _ = core.LLVMBuildCondBr(builder, cond, then_bb, else_bb);
                    core.LLVMPositionBuilderAtEnd(builder, then_bb);

                    const stack_len_before = stack.items.len;

                    var then_stack = try stack.clone(allocator);
                    defer then_stack.deinit(allocator);

                    const then_result = try compile(
                        ctx,
                        func_ctx,
                        locals,
                        &then_stack,
                        instrs,
                        then_bb,
                        builder,
                    );
                    if (!then_result.terminated) _ = core.LLVMBuildBr(builder, merge_bb);

                    var else_stack = try stack.clone(allocator);
                    defer else_stack.deinit(allocator);

                    if (then_result.exit == .@"else") {
                        const else_result = try compile(
                            ctx,
                            func_ctx,
                            locals,
                            &else_stack,
                            instrs,
                            else_bb,
                            builder,
                        );
                        if (!else_result.terminated) _ = core.LLVMBuildBr(builder, merge_bb);
                    } else {
                        core.LLVMPositionBuilderAtEnd(builder, else_bb);
                        _ = core.LLVMBuildBr(builder, merge_bb);
                    }

                    core.LLVMPositionBuilderAtEnd(builder, merge_bb);
                    const values_produced = then_stack.items.len - stack_len_before;

                    if (then_result.exit != .@"else") {
                        var i: usize = 0;
                        while (i < values_produced) : (i += 1) {
                            const then_val = then_stack.items[stack_len_before + i];
                            try stack.append(allocator, then_val);
                        }
                        continue;
                    }

                    for (0..values_produced) |i| {
                        const then_val = then_stack.items[stack_len_before + i];
                        const else_val = else_stack.items[stack_len_before + i];

                        const phi = core.LLVMBuildPhi(builder, core.LLVMTypeOf(then_val), "");

                        var values = [_]types.LLVMValueRef{ then_val, else_val };
                        var blocks = [_]types.LLVMBasicBlockRef{ then_bb, else_bb };

                        core.LLVMAddIncoming(phi, &values, &blocks, 2);

                        try stack.append(allocator, phi);
                    }
                },

                .@"return" => {
                    if (has_return) {
                        const ret_val = stack.pop().?;
                        _ = core.LLVMBuildRet(builder, ret_val);
                    } else {
                        _ = core.LLVMBuildRetVoid(builder);
                    }
                    block_terminated = true;
                },

                .end => return .{ .exit = .end, .terminated = block_terminated },

                .@"else" => return .{ .exit = .@"else", .terminated = block_terminated },

                .@"local.get" => |idx| {
                    const param_count = core.LLVMCountParams(func);

                    if (idx + 1 < param_count) {
                        const local = core.LLVMGetParam(func, @intCast(idx + 1));
                        try stack.append(allocator, local);
                        continue;
                    }
                    const local_ptr = locals[idx - (param_count - 1)];
                    const pointee_type = core.LLVMGetAllocatedType(local_ptr);
                    const value = core.LLVMBuildLoad2(builder, pointee_type, local_ptr, "");
                    try stack.append(allocator, value);
                },

                .@"i32.const" => |value| {
                    const const_val = core.LLVMConstInt(
                        core.LLVMInt32TypeInContext(llvm_ctx),
                        @intCast(value),
                        0,
                    );
                    try stack.append(allocator, const_val);
                },

                .@"i64.const" => |value| {
                    const const_val = core.LLVMConstInt(
                        core.LLVMInt64TypeInContext(llvm_ctx),
                        @intCast(value),
                        0,
                    );
                    try stack.append(allocator, const_val);
                },

                .@"f32.const" => |value| {
                    const const_val = core.LLVMConstReal(
                        core.LLVMFloatTypeInContext(llvm_ctx),
                        value,
                    );
                    try stack.append(allocator, const_val);
                },

                .@"f64.const" => |value| {
                    const const_val = core.LLVMConstReal(
                        core.LLVMDoubleTypeInContext(llvm_ctx),
                        value,
                    );
                    try stack.append(allocator, const_val);
                },

                .@"i32.eqz", .@"i64.eqz" => {
                    const val = stack.pop().?;
                    const val_type = core.LLVMTypeOf(val);
                    const zero = core.LLVMConstInt(val_type, 0, 0);

                    const result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntEQ,
                        val,
                        zero,
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
        unreachable;
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
