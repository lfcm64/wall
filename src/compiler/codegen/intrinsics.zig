const llvm = @import("llvm");

const types = llvm.types;
const core = llvm.core;

const Module = types.LLVMModuleRef;
const Context = types.LLVMContextRef;
const Value = types.LLVMValueRef;

pub const IntrinsicKind = enum {
    @"llvm.ctlz.i32",
    @"llvm.cttz.i32",
    @"llvm.ctpop.i32",
    @"llvm.fshl.i32",
    @"llvm.fshr.i32",

    @"llvm.ctlz.i64",
    @"llvm.cttz.i64",
    @"llvm.ctpop.i64",
    @"llvm.fshl.i64",
    @"llvm.fshr.i64",

    @"llvm.fabs.f32",
    @"llvm.ceil.f32",
    @"llvm.floor.f32",
    @"llvm.trunc.f32",
    @"llvm.nearbyint.f32",
    @"llvm.sqrt.f32",
    @"llvm.minnum.f32",
    @"llvm.maxnum.f32",
    @"llvm.copysign.f32",

    @"llvm.fabs.f64",
    @"llvm.ceil.f64",
    @"llvm.floor.f64",
    @"llvm.trunc.f64",
    @"llvm.nearbyint.f64",
    @"llvm.sqrt.f64",
    @"llvm.minnum.f64",
    @"llvm.maxnum.f64",
    @"llvm.copysign.f64",

    @"llvm.trap",
};

pub const Intrinsics = struct {
    const cache_size = @typeInfo(IntrinsicKind).@"enum".fields.len;

    module: Module,
    ctx: Context,

    cache: [cache_size]?Value = [_]?Value{null} ** cache_size,

    pub fn init(module: Module, ctx: Context) Intrinsics {
        return .{
            .module = module,
            .ctx = ctx,
        };
    }

    pub fn get(self: *Intrinsics, comptime kind: IntrinsicKind) Value {
        const idx = @intFromEnum(kind);
        if (self.cache[idx]) |intrinsic| return intrinsic;

        const intrinsic = self.createIntrinsic(kind);
        self.cache[idx] = intrinsic;
        return intrinsic;
    }

    fn createIntrinsic(self: *Intrinsics, comptime kind: IntrinsicKind) types.LLVMValueRef {
        const name = @tagName(kind);

        const void_ty = core.LLVMVoidTypeInContext(self.ctx);
        const i1_ty = core.LLVMInt1TypeInContext(self.ctx);
        const i32_ty = core.LLVMInt32TypeInContext(self.ctx);
        const i64_ty = core.LLVMInt64TypeInContext(self.ctx);
        const f32_ty = core.LLVMFloatTypeInContext(self.ctx);
        const f64_ty = core.LLVMDoubleTypeInContext(self.ctx);

        const func_type = switch (kind) {
            .@"llvm.ctlz.i32", .@"llvm.cttz.i32" => blk: {
                var params = [_]types.LLVMTypeRef{ i32_ty, i1_ty };
                break :blk core.LLVMFunctionType(i32_ty, &params, 2, 0);
            },

            .@"llvm.ctlz.i64", .@"llvm.cttz.i64" => blk: {
                var params = [_]types.LLVMTypeRef{ i64_ty, i1_ty };
                break :blk core.LLVMFunctionType(i64_ty, &params, 2, 0);
            },

            .@"llvm.ctpop.i32" => blk: {
                var params = [_]types.LLVMTypeRef{i32_ty};
                break :blk core.LLVMFunctionType(i32_ty, &params, 1, 0);
            },

            .@"llvm.ctpop.i64" => blk: {
                var params = [_]types.LLVMTypeRef{i64_ty};
                break :blk core.LLVMFunctionType(i64_ty, &params, 1, 0);
            },

            .@"llvm.fshl.i32", .@"llvm.fshr.i32" => blk: {
                var params = [_]types.LLVMTypeRef{i32_ty} ** 3;
                break :blk core.LLVMFunctionType(i32_ty, &params, 3, 0);
            },

            .@"llvm.fshl.i64", .@"llvm.fshr.i64" => blk: {
                var params = [_]types.LLVMTypeRef{i64_ty} ** 3;
                break :blk core.LLVMFunctionType(i64_ty, &params, 3, 0);
            },

            .@"llvm.fabs.f32",
            .@"llvm.ceil.f32",
            .@"llvm.floor.f32",
            .@"llvm.trunc.f32",
            .@"llvm.nearbyint.f32",
            .@"llvm.sqrt.f32",
            => blk: {
                var params = [_]types.LLVMTypeRef{f32_ty};
                break :blk core.LLVMFunctionType(f32_ty, &params, 1, 0);
            },

            .@"llvm.minnum.f32",
            .@"llvm.maxnum.f32",
            .@"llvm.copysign.f32",
            => blk: {
                var params = [_]types.LLVMTypeRef{f32_ty} ** 2;
                break :blk core.LLVMFunctionType(f32_ty, &params, 2, 0);
            },

            .@"llvm.fabs.f64",
            .@"llvm.ceil.f64",
            .@"llvm.floor.f64",
            .@"llvm.trunc.f64",
            .@"llvm.nearbyint.f64",
            .@"llvm.sqrt.f64",
            => blk: {
                var params = [_]types.LLVMTypeRef{f64_ty};
                break :blk core.LLVMFunctionType(f64_ty, &params, 1, 0);
            },

            .@"llvm.minnum.f64",
            .@"llvm.maxnum.f64",
            .@"llvm.copysign.f64",
            => blk: {
                var params = [_]types.LLVMTypeRef{f64_ty} ** 2;
                break :blk core.LLVMFunctionType(f64_ty, &params, 2, 0);
            },

            .@"llvm.trap" => core.LLVMFunctionType(void_ty, null, 0, 0),
        };
        return core.LLVMAddFunction(self.module, name.ptr, func_type);
    }
};
