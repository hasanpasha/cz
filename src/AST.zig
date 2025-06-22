const std = @import("std");
const cz = @import("root.zig");
const Token = cz.Lexer.Token;
const meta = @import("meta.zig");
const AcceptParentFn = meta.AcceptParentFn;

pub const Program = struct {
    function: Function,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: Program) void {
        self.arena.deinit();
    }
};

pub const Function = struct {
    name: Token,
    body: Statement,
};

pub const Statement = union(enum) {
    @"return": Return,

    pub const accept = AcceptParentFn(@This());
};

pub const Unary = struct {
    operator: Token,
    expr: *Expression,
};

pub const Constant = struct {
    tok: Token,
};

pub const Expression = union(enum) {
    constant: Constant,
    unary: Unary,

    pub const accept = AcceptParentFn(@This());

    pub fn create(self: Expression, allocator: std.mem.Allocator) !*Expression {
        const ptr = try allocator.create(Expression);
        ptr.* = self;
        return ptr;
    }
};

pub const Return = struct {
    keyword: Token,
    expr: Expression,
};
