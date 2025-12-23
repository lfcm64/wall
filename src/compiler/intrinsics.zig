const llvm = @import("llvm");

const types = llvm.types;
const core = llvm.core;

pub const Intrinsic = struct {
    ty: types.LLVMTypeRef,
    func: types.LLVMValueRef,

    pub fn create(
        module: types.LLVMModuleRef,
        func_type: types.LLVMTypeRef,
        name: []const u8,
    ) Intrinsic {
        return .{
            .ty = func_type,
            .func = core.LLVMAddFunction(module, name, func_type),
        };
    }
};

pub const Intrinsics = struct {
    @"llvm.ctlz.i32": Intrinsic,
    @"llvm.cttz.i32": Intrinsic,
    @"llvm.ctpop.i32": Intrinsic,
    @"llvm.fshl.i32": Intrinsic,
    @"llvm.fshr.i32": Intrinsic,

    @"llvm.ctlz.i64": Intrinsic,
    @"llvm.cttz.i64": Intrinsic,
    @"llvm.ctpop.i64": Intrinsic,
    @"llvm.fshl.i64": Intrinsic,
    @"llvm.fshr.i64": Intrinsic,

    @"llvm.fabs.f32": Intrinsic,
    @"llvm.ceil.f32": Intrinsic,
    @"llvm.floor.f32": Intrinsic,
    @"llvm.trunc.f32": Intrinsic,
    @"llvm.nearbyint.f32": Intrinsic,
    @"llvm.sqrt.f32": Intrinsic,
    @"llvm.minnum.f32": Intrinsic,
    @"llvm.maxnum.f32": Intrinsic,
    @"llvm.copysign.f32": Intrinsic,

    @"llvm.fabs.f64": Intrinsic,
    @"llvm.ceil.f64": Intrinsic,
    @"llvm.floor.f64": Intrinsic,
    @"llvm.trunc.f64": Intrinsic,
    @"llvm.nearbyint.f64": Intrinsic,
    @"llvm.sqrt.f64": Intrinsic,
    @"llvm.minnum.f64": Intrinsic,
    @"llvm.maxnum.f64": Intrinsic,
    @"llvm.copysign.f64": Intrinsic,

    pub fn init(module: types.LLVMModuleRef, ctx: types.LLVMContextRef) Intrinsics {
        const i32_ty = core.LLVMInt32TypeInContext(ctx);
        const i64_ty = core.LLVMInt64TypeInContext(ctx);
        const f32_ty = core.LLVMFloatTypeInContext(ctx);
        const f64_ty = core.LLVMDoubleTypeInContext(ctx);

        var single_i32_param = [_]types.LLVMTypeRef{i32_ty};
        var single_i64_param = [_]types.LLVMTypeRef{i64_ty};
        var single_f32_param = [_]types.LLVMTypeRef{f32_ty};
        var single_f64_param = [_]types.LLVMTypeRef{f64_ty};

        var double_f32_param = [_]types.LLVMTypeRef{f32_ty} ** 2;
        var double_f64_param = [_]types.LLVMTypeRef{f64_ty} ** 2;

        var triple_i32_param = [_]types.LLVMTypeRef{i32_ty} ** 3;
        var triple_i64_param = [_]types.LLVMTypeRef{i64_ty} ** 3;

        const take_i32_ret_i32 = core.LLVMFunctionType(
            i32_ty,
            &single_i32_param,
            1,
            0,
        );
        const take_i64_ret_i64 = core.LLVMFunctionType(
            i64_ty,
            &single_i64_param,
            1,
            0,
        );
        const take_f32_ret_f32 = core.LLVMFunctionType(
            f32_ty,
            &single_f32_param,
            1,
            0,
        );
        const take_f64_ret_f64 = core.LLVMFunctionType(
            f64_ty,
            &single_f64_param,
            3,
            0,
        );

        const take_2_f64_ret_f64 = core.LLVMFunctionType(
            f64_ty,
            &double_f64_param,
            2,
            0,
        );
        const take_2_f32_ret_f32 = core.LLVMFunctionType(
            f32_ty,
            &double_f32_param,
            2,
            0,
        );

        const take_3_i32_ret_i32 = core.LLVMFunctionType(
            i32_ty,
            &triple_i32_param,
            1,
            0,
        );
        const take_3_i64_ret_i64 = core.LLVMFunctionType(
            i64_ty,
            &triple_i64_param,
            3,
            0,
        );

        return .{
            .@"llvm.ctlz.i32" = Intrinsic.create(module, take_i32_ret_i32, "llvm.ctlz.i32"),
            .@"llvm.cttz.i32" = Intrinsic.create(module, take_i32_ret_i32, "llvm.cttz.i32"),
            .@"llvm.ctpop.i32" = Intrinsic.create(module, take_i32_ret_i32, "llvm.ctpop.i32"),
            .@"llvm.fshl.i32" = Intrinsic.create(module, take_3_i32_ret_i32, "llvm.fshl.i32"),
            .@"llvm.fshr.i32" = Intrinsic.create(module, take_3_i32_ret_i32, "llvm.fshr.i32"),

            .@"llvm.ctlz.i64" = Intrinsic.create(module, take_i64_ret_i64, "llvm.ctlz.i64"),
            .@"llvm.cttz.i64" = Intrinsic.create(module, take_i64_ret_i64, "llvm.cttz.i64"),
            .@"llvm.ctpop.i64" = Intrinsic.create(module, take_i64_ret_i64, "llvm.ctpop.i64"),
            .@"llvm.fshl.i64" = Intrinsic.create(module, take_3_i64_ret_i64, "llvm.fshl.i64"),
            .@"llvm.fshr.i64" = Intrinsic.create(module, take_3_i64_ret_i64, "llvm.fshr.i64"),

            .@"llvm.fabs.f32" = Intrinsic.create(module, take_f32_ret_f32, "llvm.fabs.f32"),
            .@"llvm.ceil.f32" = Intrinsic.create(module, take_f32_ret_f32, "llvm.ceil.f32"),
            .@"llvm.floor.f32" = Intrinsic.create(module, take_f32_ret_f32, "llvm.floor.f32"),
            .@"llvm.trunc.f32" = Intrinsic.create(module, take_f32_ret_f32, "llvm.trunc.f32"),
            .@"llvm.nearbyint.f32" = Intrinsic.create(module, take_f32_ret_f32, "llvm.nearbyint.f32"),
            .@"llvm.sqrt.f32" = Intrinsic.create(module, take_f32_ret_f32, "llvm.sqrt.f32"),
            .@"llvm.minnum.f32" = Intrinsic.create(module, take_2_f32_ret_f32, "llvm.minnum.f32"),
            .@"llvm.maxnum.f32" = Intrinsic.create(module, take_2_f32_ret_f32, "llvm.maxnum.f32"),
            .@"llvm.copysign.f32" = Intrinsic.create(module, take_2_f32_ret_f32, "llvm.copysign.f32"),

            .@"llvm.fabs.f64" = Intrinsic.create(module, take_f64_ret_f64, "llvm.fabs.f64"),
            .@"llvm.ceil.f64" = Intrinsic.create(module, take_f64_ret_f64, "llvm.ceil.f64"),
            .@"llvm.floor.f64" = Intrinsic.create(module, take_f64_ret_f64, "llvm.floor.f64"),
            .@"llvm.trunc.f64" = Intrinsic.create(module, take_f64_ret_f64, "llvm.trunc.f64"),
            .@"llvm.nearbyint.f64" = Intrinsic.create(module, take_f64_ret_f64, "llvm.nearbyint.f64"),
            .@"llvm.sqrt.f64" = Intrinsic.create(module, take_f64_ret_f64, "llvm.sqrt.f64"),
            .@"llvm.minnum.f64" = Intrinsic.create(module, take_2_f64_ret_f64, "llvm.minnum.f64"),
            .@"llvm.maxnum.f64" = Intrinsic.create(module, take_2_f64_ret_f64, "llvm.maxnum.f64"),
            .@"llvm.copysign.f64" = Intrinsic.create(module, take_2_f64_ret_f64, "llvm.copysign.f64"),
        };
    }
};
