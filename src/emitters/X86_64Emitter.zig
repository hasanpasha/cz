const std = @import("std");
const cz = @import("../root.zig");
const Gen = cz.AssemblyGenerator;
const AssemblyEmitter = cz.AssemblyEmitter;

pub const X86_64Emitter = @This();

writer: std.io.AnyWriter,

pub fn emitter(self: *X86_64Emitter) AssemblyEmitter {
    return AssemblyEmitter{
        .ctx = self,
        .vtable = .{
            .emit = &emit,
        },
    };
}

fn emit(ctx: ?*anyopaque, prog: Gen.AssemblyProgram) AssemblyEmitter.Error!void {
    var self: *X86_64Emitter = @ptrCast(@alignCast(ctx));
    try self.accept_program(prog);
}

fn accept_program(self: X86_64Emitter, program: Gen.AssemblyProgram) !void {
    // try self.writer.writeAll(".section .text\n");
    try self.accept_function_def(program.function_definition);
    try self.writer.writeAll(".section .note.GNU-stack,\"\",@progbits\n");
}

fn accept_function_def(self: X86_64Emitter, fun: Gen.FunctionDefinition) !void {
    try self.writer.print(".global {s}\n", .{fun.name});
    try self.writer.print("{s}:\n", .{fun.name});
    try self.accept_instructions(fun.instructions);
}

pub fn accept_ret(self: X86_64Emitter, ret: Gen.ReturnInstruction) !void {
    _ = ret;
    try self.writer.writeAll("ret");
}

pub fn accept_mov(self: X86_64Emitter, mov: Gen.MoveInstruction) !void {
    try self.writer.writeAll("movl ");
    try mov.src.accept(self, void);
    try self.writer.writeAll(", ");
    try mov.dst.accept(self, void);
}

pub fn accept_instructions(self: X86_64Emitter, instrs: Gen.Instructions) !void {
    for (instrs.items) |instr| {
        try self.writer.writeAll("  ");
        try instr.accept(self, void);
        try self.writer.writeByte('\n');
    }
}

pub fn accept_register(self: X86_64Emitter, reg: Gen.Register) !void {
    try self.writer.print("%{s}", .{reg.name});
}

pub fn accept_immediate(self: X86_64Emitter, imm: Gen.Immediate) !void {
    try self.writer.print("${s}", .{imm.value});
}
