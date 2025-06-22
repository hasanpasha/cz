const std = @import("std");
const cz = @import("root.zig");
const Parser = cz.Parser;
const Token = cz.Lexer.Token;
const AcceptParentFn = @import("meta.zig").AcceptParentFn;

pub const Program = struct {
    function_definition: Function,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: Program) void {
        self.arena.deinit();
    }

    pub fn format(self: Program, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;

        if (fmt[0] == 'p') {
            const printer = PrettyPrinter{ .writer = writer };
            try printer.print(self);
        } else {
            try writer.print("TackyIR{ .function_definition = {} }", .{self.function_definition});
        }
    }
};

pub const Function = struct {
    identifier: Token,
    body: []Instruction,
};

pub const InstructionKind = enum {
    @"return",
    unary,
};

pub const Instruction = union(InstructionKind) {
    @"return": Return,
    unary: Unary,

    pub const accept = AcceptParentFn(@This());
};

pub const Return = struct {
    value: Value,
};

pub const UnaryOperator = enum {
    complement,
    negate,
};

pub const Unary = struct {
    operator: UnaryOperator,
    src: Value,
    dst: Value,
};

pub const ValueKind = enum {
    constant,
    variable,
};

pub const Value = union(ValueKind) {
    constant: Constant,
    variable: Variable,

    pub const accept = AcceptParentFn(@This());
};

pub const Constant = struct {
    tok: Token,
};

pub const Variable = struct {
    name: []const u8,
};

pub const PrettyPrinter = struct {
    writer: std.io.AnyWriter,

    pub fn print(self: PrettyPrinter, program: Program) !void {
        try self.writer.print("ProgramIR(", .{});
        try self.accept_function(program.function_definition);
        try self.writer.print(")", .{});
    }

    pub fn accept_function(self: PrettyPrinter, function: Function) !void {
        try self.writer.print("Function({s}, ", .{function.identifier.value.identifier});
        try self.accept_instructions(function.body);
        try self.writer.print(")", .{});
    }

    pub fn accept_instructions(self: PrettyPrinter, instrs: []Instruction) !void {
        for (instrs, 0..) |instr, i| {
            try instr.accept(self, void);
            if (i + 1 < instrs.len) {
                try self.writer.writeAll(", ");
            }
        }
    }

    pub fn accept_constant(self: PrettyPrinter, constant: Constant) !void {
        try self.writer.print("Constant({s})", .{constant.tok.value.constant});
    }

    pub fn accept_return(self: PrettyPrinter, ret: Return) !void {
        try self.writer.print("Return(", .{});
        try ret.value.accept(self, void);
        try self.writer.print(")", .{});
    }

    pub fn accept_unary(self: PrettyPrinter, unary: Unary) !void {
        try self.writer.print("Unary({}, ", .{unary.operator});
        try unary.src.accept(self, void);
        try self.writer.print(", ", .{});
        try unary.dst.accept(self, void);
        try self.writer.print(")", .{});
    }

    pub fn accept_variable(self: PrettyPrinter, variable: Variable) !void {
        try self.writer.print("Var({s})", .{variable.name});
    }
};
