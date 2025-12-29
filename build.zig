const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const llvm_dep = b.dependency("llvm", .{
        .target = target,
        .optimize = optimize,
    });
    const llvm_mod = llvm_dep.module("llvm");

    const wasm_mod = b.addModule("wasm", .{
        .root_source_file = b.path("src/wasm/wasm.zig"),
        .target = target,
        .optimize = optimize,
    });

    const mod = b.addModule("wall", .{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "wall",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "wall", .module = mod },
            },
        }),
    });

    exe.root_module.addImport("wasm", wasm_mod);
    exe.root_module.addImport("llvm", llvm_mod);
    b.installArtifact(exe);

    const run_step = b.step("run", "Run the app");

    const run_cmd = b.addRunArtifact(exe);
    run_step.dependOn(&run_cmd.step);

    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
}
