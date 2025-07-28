const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const mod = b.addModule("args", .{
        .root_source_file = b.path("src/args.zig"),
        .target = target,
        .optimize = optimize,
    });

    const mod_unit_tests = b.addTest(.{ .root_module = mod });
    const run_mod_unit_tests = b.addRunArtifact(mod_unit_tests);

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_mod_unit_tests.step);
}
