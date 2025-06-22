const std = @import("std");
const cz = @import("root.zig");
const Lexer = cz.Lexer;
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;
const meta = @import("meta.zig");
const AcceptParentFn = meta.AcceptParentFn;

const stderr = std.io.getStdErr().writer();

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

pub const Constant = struct {
    tok: Token,
};

pub const Expression = union(enum) {
    constant: Constant,

    pub const accept = AcceptParentFn(@This());
};

pub const Return = struct {
    keyword: Token,
    expr: Expression,
};

pub const PrettyPrinter = struct {
    writer: std.io.AnyWriter,

    pub fn print(self: PrettyPrinter, program: Program) !void {
        try self.writer.print("Program(", .{});
        try self.accept_function(program.function);
        try self.writer.print(")\n", .{});
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

    pub fn accept_constant(self: PrettyPrinter, constant: Constant) !void {
        try self.writer.print("Constant({s})", .{constant.tok.value.constant});
    }
};

const Parser = @This();

index: usize = 0,
tokens: []const Token,
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator, tokens: []const Token) !*Parser {
    const parser = try allocator.create(Parser);

    parser.* = .{
        .tokens = tokens,
        .allocator = allocator,
    };

    return parser;
}

pub fn deinit(self: *Parser) void {
    self.allocator.destroy(self);
}

pub fn parse_program(self: *Parser) !Program {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    const cur_allocator = self.allocator;
    self.allocator = arena.allocator();
    defer self.allocator = cur_allocator;

    const func = try self.function();
    _ = try self.consume(.eoi, "Expect end of input.");

    return Program{ .function = func, .arena = arena };
}

fn function(self: *Parser) !Function {
    _ = try self.consume(.int, "Expect `int` at start of function definition.");
    const name = try self.consume(.identifier, "Expect an identifier for function definition.");
    _ = try self.consume(.left_paren, "expect a '(' after identifier.");
    _ = try self.consume(.void, "expect a `void` as argument.");
    _ = try self.consume(.right_paren, "expect a ')' after arguments list.");
    _ = try self.consume(.left_brace, "expect '{' opening a function body.");
    const body = try self.statement();
    _ = try self.consume(.right_brace, "expect '}' closing a function body.");
    return Function{ .name = name, .body = body };
}

fn statement(self: *Parser) !Statement {
    return .{ .@"return" = try self.return_stmt() };
}

fn return_stmt(self: *Parser) !Return {
    const token = try self.consume(.@"return", "expect a `return` keyword.");
    const expr = try self.expression();
    _ = try self.consume(.semicolon, "Expect a ';' ending a return statement.");
    return .{ .keyword = token, .expr = expr };
}

fn expression(self: *Parser) !Expression {
    return .{ .constant = .{ .tok = try self.consume(.constant, "Expect a constant expression.") } };
}

fn consume(self: *Parser, kind: TokenKind, msg: []const u8) !Token {
    try self.assert_has_more_tokens();

    const cur = self.peek();
    if (cur.value != kind) {
        try stderr.print("is {}, {s}\n", .{ cur, msg });
        return error.unexpected_token;
    }

    return self.advance();
}

fn is_at_end(self: *Parser) bool {
    return self.index >= self.tokens.len;
}

fn assert_has_more_tokens(self: *Parser) !void {
    if (self.is_at_end()) return error.no_more_tokens;
}

fn advance(self: *Parser) Token {
    const cur = self.peek();
    self.index += 1;
    return cur;
}

fn peek(self: *Parser) Token {
    return self.tokens[self.index];
}
