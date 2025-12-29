const std = @import("std");
const llvm = @import("llvm");
const wasm = @import("../../wasm/wasm.zig");
const conv = @import("conv.zig");

const Intrinsic = @import("../intrinsics.zig").Intrinsic;
const Context = @import("../Context.zig");
const State = @import("../State.zig");

const instr = wasm.instr;
const types = llvm.types;
const core = llvm.core;

const Allocator = std.mem.Allocator;

fn createLocals(ctx: *Context, body: wasm.types.FuncBody, builder: types.LLVMBuilderRef) ![]types.LLVMValueRef {
    var locals: std.ArrayList(types.LLVMValueRef) = .{};
    errdefer locals.deinit(ctx.allocator);

    var it = body.locals.iter();

    while (try it.next()) |local| {
        const llvm_type = conv.typeToLLVM(local.valtype, ctx.llvm_context);
        const zero = core.LLVMConstNull(llvm_type);

        for (0..local.count) |_| {
            const loc = core.LLVMBuildAlloca(builder, llvm_type, "");
            _ = core.LLVMBuildStore(builder, zero, loc);
            try locals.append(ctx.allocator, loc);
        }
    }
    return locals.toOwnedSlice(ctx.allocator);
}

pub const CodeCompiler = struct {
    pub fn compile(
        ctx: *Context,
        body: wasm.types.FuncBody,
        idx: u32,
    ) !void {
        const llvm_ctx = ctx.llvm_context;
        const allocator = ctx.allocator;

        const func = ctx.llvm_funcs.items[ctx.imported_funcs + idx];
        const func_type = core.LLVMGlobalGetValueType(func);

        const entry_block = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "");
        const exit_block = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "");

        const builder = core.LLVMCreateBuilder();
        defer core.LLVMDisposeBuilder(builder);

        core.LLVMPositionBuilderAtEnd(builder, exit_block);

        const locals = try createLocals(ctx, body, builder);
        defer ctx.allocator.free(locals);

        const return_type = core.LLVMGetReturnType(func_type);
        const is_void = core.LLVMGetTypeKind(return_type) == types.LLVMTypeKind.LLVMVoidTypeKind;

        const return_phi = if (!is_void) core.LLVMBuildPhi(builder, return_type, "") else null;
        if (return_phi) |phi| _ = core.LLVMBuildRet(builder, phi) else _ = core.LLVMBuildRetVoid(builder);

        var state = State{};
        defer state.deinit(allocator);

        try state.pushFrame(allocator, .{
            .block = .{
                .next = exit_block,
                .phi = return_phi,
            },
        });

        core.LLVMPositionBuilderAtEnd(builder, entry_block);

        var instrs = instr.Iterator.init(body.code);

        while (try instrs.next()) |in| {
            if (!state.reachable and in != .end and in != .@"else") continue;

            switch (in) {
                .@"unreachable" => state.reachable = false,
                .nop => {},

                .block => |block_type| {
                    const block_body = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "");
                    const block_exit = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "");

                    _ = core.LLVMBuildBr(builder, block_body);
                    core.LLVMPositionBuilderAtEnd(builder, block_exit);
                    const phi = conv.blockTypeToPhi(block_type, llvm_ctx, builder);

                    try state.pushFrame(allocator, .{
                        .block = .{
                            .next = block_exit,
                            .phi = phi,
                        },
                    });
                    core.LLVMPositionBuilderAtEnd(builder, block_body);
                },

                .loop => |block_type| {
                    const loop_header = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "");
                    const loop_exit = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "");

                    _ = core.LLVMBuildBr(builder, loop_header);
                    core.LLVMPositionBuilderAtEnd(builder, loop_header);
                    const header_phi = conv.blockTypeToPhi(block_type, llvm_ctx, builder);

                    core.LLVMPositionBuilderAtEnd(builder, loop_exit);
                    const exit_phi = conv.blockTypeToPhi(block_type, llvm_ctx, builder);

                    try state.pushFrame(allocator, .{
                        .loop = .{
                            .header = loop_header,
                            .next = loop_exit,
                            .loop_phi = header_phi,
                            .phi = exit_phi,
                        },
                    });
                    core.LLVMPositionBuilderAtEnd(builder, loop_header);
                },

                .@"if" => |block_type| {
                    const cond = state.pop() orelse return error.StackUnderflow;

                    const zero = core.LLVMConstInt(core.LLVMInt32TypeInContext(llvm_ctx), 0, 0);
                    const cond_bool = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntNE,
                        cond,
                        zero,
                        "",
                    );

                    const then_block = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "if.then");
                    const else_block = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "if.else");
                    const merge_block = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "if.merge");

                    _ = core.LLVMBuildCondBr(builder, cond_bool, then_block, else_block);

                    core.LLVMPositionBuilderAtEnd(builder, merge_block);
                    const phi = conv.blockTypeToPhi(block_type, llvm_ctx, builder);

                    try state.pushFrame(allocator, .{
                        .if_else = .{
                            .then = then_block,
                            .@"else" = else_block,
                            .next = merge_block,
                            .phi = phi,
                        },
                    });
                    core.LLVMPositionBuilderAtEnd(builder, then_block);
                },

                .@"else" => {
                    const current_block = core.LLVMGetInsertBlock(builder);

                    const frame = state.currentFrame().?;
                    frame.if_else.has_else = true;

                    if (state.reachable) {
                        if (frame.phi()) |phi_value| {
                            const value = state.pop() orelse return error.StackUnderflow;
                            var values = [_]types.LLVMValueRef{value};
                            var blocks = [_]types.LLVMBasicBlockRef{current_block};
                            core.LLVMAddIncoming(phi_value, &values, &blocks, 1);
                        }
                    }
                    _ = core.LLVMBuildBr(builder, frame.next());
                    core.LLVMPositionBuilderAtEnd(builder, frame.if_else.@"else");
                },

                .end => {
                    var frame = state.popFrame().?;
                    const current_block = core.LLVMGetInsertBlock(builder);

                    if (state.reachable and current_block != null) {
                        if (frame.phi()) |phi_value| {
                            const value = state.pop().?;
                            var values = [_]types.LLVMValueRef{value};
                            var blocks = [_]types.LLVMBasicBlockRef{current_block.?};
                            core.LLVMAddIncoming(phi_value, &values, &blocks, 1);
                        }
                        _ = core.LLVMBuildBr(builder, frame.next());
                    }

                    if (frame == .if_else and !frame.if_else.has_else) {
                        core.LLVMPositionBuilderAtEnd(builder, frame.if_else.@"else");
                        _ = core.LLVMBuildBr(builder, frame.if_else.next);
                    }

                    core.LLVMPositionBuilderAtEnd(builder, frame.next());
                    state.reachable = true;

                    const function_end = state.control_stack.items.len == 0;
                    if (!function_end) {
                        if (frame.phi()) |phi_value| try state.push(allocator, phi_value);
                    }
                },

                .br => |label_idx| {
                    const current_block = core.LLVMGetInsertBlock(builder);

                    const frame = state.frameAtDepth(label_idx).?;
                    const target = frame.brDest();

                    if (frame.phi()) |phi_value| {
                        const value = state.pop().?;
                        var values = [_]types.LLVMValueRef{value};
                        var blocks = [_]types.LLVMBasicBlockRef{current_block};
                        core.LLVMAddIncoming(phi_value, &values, &blocks, 1);
                    }

                    _ = core.LLVMBuildBr(builder, target);
                    state.reachable = false;
                },

                .br_if => |label_idx| {
                    const cond = state.pop() orelse return error.StackUnderflow;

                    const zero = core.LLVMConstInt(core.LLVMInt32TypeInContext(llvm_ctx), 0, 0);
                    const cond_bool = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntNE,
                        cond,
                        zero,
                        "",
                    );

                    const current_block = core.LLVMGetInsertBlock(builder);

                    const frame = state.frameAtDepth(label_idx).?;
                    const target = frame.brDest();

                    if (frame.phi()) |phi_value| {
                        const value = state.pop().?;
                        var values = [_]types.LLVMValueRef{value};
                        var blocks = [_]types.LLVMBasicBlockRef{current_block};
                        core.LLVMAddIncoming(phi_value, &values, &blocks, 1);
                    }

                    const else_block = core.LLVMAppendBasicBlockInContext(llvm_ctx, func, "");

                    _ = core.LLVMBuildCondBr(builder, cond_bool, target, else_block);
                    core.LLVMPositionBuilderAtEnd(builder, else_block);
                },

                .@"return" => {
                    const current_block = core.LLVMGetInsertBlock(builder).?;
                    const func_frame = &state.control_stack.items[0];

                    if (func_frame.phi()) |phi_value| {
                        const value = state.pop() orelse return error.StackUnderflow;
                        var values = [_]types.LLVMValueRef{value};
                        var blocks = [_]types.LLVMBasicBlockRef{current_block};
                        core.LLVMAddIncoming(phi_value, &values, &blocks, 1);
                    }
                    _ = core.LLVMBuildBr(builder, func_frame.next());
                    state.reachable = false;
                },

                .call => |func_idx| {
                    const callee = ctx.llvm_funcs.items[func_idx];
                    const callee_type = core.LLVMGlobalGetValueType(callee);

                    const param_count = core.LLVMCountParams(callee);

                    const args = try allocator.alloc(types.LLVMValueRef, param_count);
                    defer allocator.free(args);

                    args[0] = core.LLVMGetParam(func, 0);

                    var i: usize = param_count - 1;
                    while (i > 0) : (i -= 1) args[i] = state.stack.pop().?;

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
                        try state.push(allocator, call_result);
                    }
                },

                .@"local.get" => |local_idx| {
                    const param_count = core.LLVMCountParams(func);

                    if (local_idx + 1 < param_count) {
                        const local = core.LLVMGetParam(func, @intCast(local_idx + 1));
                        try state.push(allocator, local);
                        continue;
                    }
                    const local_ptr = locals[local_idx - (param_count - 1)];
                    const pointee_type = core.LLVMGetAllocatedType(local_ptr);
                    const value = core.LLVMBuildLoad2(builder, pointee_type, local_ptr, "");
                    try state.push(allocator, value);
                },

                .@"i32.const" => |value| {
                    const const_val = core.LLVMConstInt(
                        core.LLVMInt32TypeInContext(llvm_ctx),
                        @intCast(value),
                        0,
                    );
                    try state.push(allocator, const_val);
                },

                .@"i64.const" => |value| {
                    const const_val = core.LLVMConstInt(
                        core.LLVMInt64TypeInContext(llvm_ctx),
                        @intCast(value),
                        0,
                    );
                    try state.push(allocator, const_val);
                },

                .@"f32.const" => |value| {
                    const const_val = core.LLVMConstReal(
                        core.LLVMFloatTypeInContext(llvm_ctx),
                        value,
                    );
                    try state.push(allocator, const_val);
                },

                .@"f64.const" => |value| {
                    const const_val = core.LLVMConstReal(
                        core.LLVMDoubleTypeInContext(llvm_ctx),
                        value,
                    );
                    try state.push(allocator, const_val);
                },

                .@"i32.eqz", .@"i64.eqz" => {
                    const val = state.pop().?;
                    const val_type = core.LLVMTypeOf(val);
                    const zero = core.LLVMConstInt(val_type, 0, 0);
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntEQ,
                        val,
                        zero,
                        "",
                    );
                    const result = core.LLVMBuildZExt(
                        builder,
                        cmp_result,
                        core.LLVMInt32TypeInContext(llvm_ctx),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"i32.eq", .@"i64.eq" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntEQ,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"i32.ne", .@"i64.ne" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntNE,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"i32.lt_s", .@"i64.lt_s" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntSLT,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"i32.lt_u", .@"i64.lt_u" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntULT,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"i32.gt_s", .@"i64.gt_s" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntSGT,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"i32.gt_u", .@"i64.gt_u" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntUGT,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"i32.le_s", .@"i64.le_s" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntSLE,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"i32.le_u", .@"i64.le_u" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntULE,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"i32.ge_s", .@"i64.ge_s" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntSGE,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"i32.ge_u", .@"i64.ge_u" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildICmp(
                        builder,
                        types.LLVMIntPredicate.LLVMIntUGE,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"f32.eq", .@"f64.eq" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealOEQ,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"f32.ne", .@"f64.ne" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealONE,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"f32.lt", .@"f64.lt" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealOLT,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"f32.gt", .@"f64.gt" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealOGT,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"f32.le", .@"f64.le" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealOLE,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"f32.ge", .@"f64.ge" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const cmp_result = core.LLVMBuildFCmp(
                        builder,
                        types.LLVMRealPredicate.LLVMRealOGE,
                        arg1,
                        arg2,
                        "",
                    );
                    const i32_type = core.LLVMInt32TypeInContext(llvm_ctx);
                    const result = core.LLVMBuildZExt(builder, cmp_result, i32_type, "");
                    try state.push(allocator, result);
                },

                .@"i32.clz" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{
                        val,
                        core.LLVMConstInt(core.LLVMInt1Type(), 0, 0),
                    };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ctlz.i32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"i32.ctz" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{
                        val,
                        core.LLVMConstInt(core.LLVMInt1Type(), 0, 0),
                    };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.cttz.i32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"i32.popcnt" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ctpop.i32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"i64.clz" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{
                        val,
                        core.LLVMConstInt(core.LLVMInt1Type(), 0, 0),
                    };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ctlz.i64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"i64.ctz" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{
                        val,
                        core.LLVMConstInt(core.LLVMInt1Type(), 0, 0),
                    };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.cttz.i64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"i64.popcnt" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ctpop.i64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"i32.add", .@"i64.add" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildAdd(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.sub", .@"i64.sub" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildSub(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.mul", .@"i64.mul" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildMul(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.div_s", .@"i64.div_s" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildSDiv(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.div_u", .@"i64.div_u" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildUDiv(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.rem_s", .@"i64.rem_s" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildSRem(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.rem_u", .@"i64.rem_u" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildURem(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.and", .@"i64.and" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildAnd(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.or", .@"i64.or" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildOr(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.xor", .@"i64.xor" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildXor(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.shl", .@"i64.shl" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildShl(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.shr_s", .@"i64.shr_s" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildAShr(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.shr_u", .@"i64.shr_u" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildLShr(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"i32.rotl" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fshl.i32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"i32.rotr" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fshr.i32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"i64.rotl" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fshl.i64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"i64.rotr" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fshr.i64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f32.abs" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fabs.f32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f32.neg", .@"f64.neg" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFNeg(builder, val, "");
                    try state.push(allocator, result);
                },

                .@"f32.ceil" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ceil.f32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f32.floor" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.floor.f32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f32.trunc" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.trunc.f32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f32.nearest" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.nearbyint.f32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f32.sqrt" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.sqrt.f32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f32.min" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.minnum.f32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f32.max" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.maxnum.f32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f32.copysign" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.copysign.f32",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f64.abs" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.fabs.f64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f64.ceil" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.ceil.f64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f64.floor" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.floor.f64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f64.trunc" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.trunc.f64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f64.nearest" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.nearbyint.f64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f64.sqrt" => {
                    const val = state.pop().?;
                    var args = [_]types.LLVMValueRef{val};
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.sqrt.f64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f32.add", .@"f64.add" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildFAdd(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"f32.sub", .@"f64.sub" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildFSub(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"f32.mul", .@"f64.mul" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildFMul(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"f32.div", .@"f64.div" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    const result = core.LLVMBuildFDiv(builder, arg1, arg2, "");
                    try state.push(allocator, result);
                },

                .@"f64.min" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.minnum.f64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f64.max" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.maxnum.f64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"f64.copysign" => {
                    const arg2 = state.pop().?;
                    const arg1 = state.pop().?;
                    var args = [_]types.LLVMValueRef{ arg1, arg2 };
                    const result = buildIntrinsicCall(
                        builder,
                        ctx.intrinsics.@"llvm.copysign.f64",
                        &args,
                    );
                    try state.push(allocator, result);
                },

                .@"i32.wrap_i64" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildTrunc(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"i32.trunc_f32_s" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFPToSI(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"i32.trunc_f32_u" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFPToUI(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"i32.trunc_f64_s" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFPToSI(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"i32.trunc_f64_u" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFPToUI(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"i64.extend_i32_s" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildSExt(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"i64.extend_i32_u" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildZExt(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"i64.trunc_f32_s" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFPToSI(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },
                .@"i64.trunc_f32_u" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFPToUI(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },
                .@"i64.trunc_f64_s" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFPToSI(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },
                .@"i64.trunc_f64_u" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFPToUI(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f32.convert_i32_s" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildSIToFP(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f32.convert_i32_u" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildUIToFP(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f32.convert_i64_s" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildSIToFP(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f32.convert_i64_u" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildUIToFP(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f32.demote_f64" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFPTrunc(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f64.convert_i32_s" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildSIToFP(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f64.convert_i32_u" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildUIToFP(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f64.convert_i64_s" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildSIToFP(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f64.convert_i64_u" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildUIToFP(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f64.promote_f32" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildFPExt(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"i32.reinterpret_f32" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildBitCast(
                        builder,
                        val,
                        core.LLVMInt32Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f32.reinterpret_i32" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildBitCast(
                        builder,
                        val,
                        core.LLVMFloatType(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"i64.reinterpret_f64" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildBitCast(
                        builder,
                        val,
                        core.LLVMInt64Type(),
                        "",
                    );
                    try state.push(allocator, result);
                },

                .@"f64.reinterpret_i64" => {
                    const val = state.pop().?;
                    const result = core.LLVMBuildBitCast(
                        builder,
                        val,
                        core.LLVMDoubleType(),
                        "",
                    );
                    try state.push(allocator, result);
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
