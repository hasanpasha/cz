const std = @import("std");
const cz = @import("root.zig");
const IR = cz.TackyIR;

pub const ASMProgramError = anyerror;

pub const ASMProgram = struct {
    ctx: ?*anyopaque,
    emit_fn: *const fn (ctx: ?*anyopaque, writer: std.io.AnyWriter) ASMProgramError!void,
    destroy_fn: *const fn (ctx: ?*anyopaque) void,
    pretty_print_fn: ?*const fn (ctx: ?*anyopaque, writer: std.io.AnyWriter) ASMProgramError!void,

    pub fn emit(self: ASMProgram, writer: std.io.AnyWriter) ASMProgramError!void {
        try self.emit_fn(self.ctx, writer);
    }

    pub fn pretty_print(self: ASMProgram, writer: std.io.AnyWriter) ASMProgramError!void {
        if (self.pretty_print_fn == null) return error.not_implemented;
        try self.pretty_print_fn.?(self.ctx, writer);
    }

    pub fn destroy(self: ASMProgram) void {
        // self.destroy_fn(self.ctx);
        const destroy_fn = self.destroy_fn;
        destroy_fn(self.ctx);
    }

    pub fn format(self: ASMProgram, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try self.pretty_print(writer);
    }
};

pub const Error = anyerror;

const ASMGenerator = @This();

ctx: ?*anyopaque,
generate_fn: *const fn (ctx: ?*anyopaque, ir_program: IR.Program) Error!ASMProgram,

pub fn generate(self: ASMGenerator, ir_program: IR.Program) Error!ASMProgram {
    return try self.generate_fn(self.ctx, ir_program);
}
