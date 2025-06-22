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

    pub fn format(self: Program, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options;

        if (fmt[0] == 'p') {
            const printer = PrettyPrinter{ .writer = writer };
            try printer.print(self);
        } else {
            try writer.print("ProgramAST{ .function = {} }", .{self.function});
        }
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

pub const PrettyPrinter = struct {
    writer: std.io.AnyWriter,

    pub fn print(self: PrettyPrinter, program: Program) !void {
        try self.writer.print("ProgramAST(", .{});
        try self.accept_function(program.function);
        try self.writer.print(")", .{});
    }

    pub fn accept_function(self: PrettyPrinter, fun: Function) !void {
        try self.writer.print("Function(name=\"{s}\", body=", .{fun.name.value.identifier});
        try fun.body.accept(self, void);
        try self.writer.print(")", .{});
    }

    pub fn accept_return(self: PrettyPrinter, stmt: Return) !void {
        try self.writer.print("Return(", .{});
        try stmt.expr.accept(self, void);
        try self.writer.print(")", .{});
    }

    pub fn accept_unary(self: PrettyPrinter, unary: Unary) !void {
        try self.writer.print("Unary({}, ", .{unary.operator.value});
        try unary.expr.accept(self, void);
    }

    pub fn accept_constant(self: PrettyPrinter, constant: Constant) !void {
        try self.writer.print("Constant({s})", .{constant.tok.value.constant});
    }
};
