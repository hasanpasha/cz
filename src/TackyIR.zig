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
