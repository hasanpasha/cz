const std = @import("std");
const cz = @import("root.zig");
const Gen = cz.AssemblyGenerator;

const AssemblyEmitter = @This();

ctx: ?*anyopaque,
vtable: VTable,

pub const Error = anyerror;

pub const VTable = struct {
    emit: *const fn (ctx: ?*anyopaque, program: Gen.AssemblyProgram) Error!void,
};

pub fn emit(self: AssemblyEmitter, program: Gen.AssemblyProgram) Error!void {
    try self.vtable.emit(self.ctx, program);
}
