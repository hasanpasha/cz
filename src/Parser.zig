const std = @import("std");
const cz = @import("root.zig");
const Lexer = cz.Lexer;
const Token = Lexer.Token;
const TokenKind = Lexer.TokenKind;
const meta = @import("meta.zig");
const AcceptParentFn = meta.AcceptParentFn;
const AST = cz.AST;

const stderr = std.io.getStdErr().writer();

pub const Error = error{
    unexpected_token,
    no_more_tokens,
    memory_error,
    writer_error,
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

pub fn parse_program(self: *Parser) Error!AST.Program {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    const cur_allocator = self.allocator;
    self.allocator = arena.allocator();
    defer self.allocator = cur_allocator;

    const func = try self.function();
    _ = try self.consume(.eoi, "Expect end of input.");

    return .{ .function = func, .arena = arena };
}

fn function(self: *Parser) Error!AST.Function {
    _ = try self.consume(.int, "Expect `int` at start of function definition.");
    const name = try self.consume(.identifier, "Expect an identifier for function definition.");
    _ = try self.consume(.left_paren, "expect a '(' after identifier.");
    _ = try self.consume(.void, "expect a `void` as argument.");
    _ = try self.consume(.right_paren, "expect a ')' after arguments list.");
    _ = try self.consume(.left_brace, "expect '{' opening a function body.");
    const body = try self.statement();
    _ = try self.consume(.right_brace, "expect '}' closing a function body.");
    return .{ .name = name, .body = body };
}

fn statement(self: *Parser) Error!AST.Statement {
    return .{ .@"return" = try self.return_stmt() };
}

fn return_stmt(self: *Parser) Error!AST.Return {
    const token = try self.consume(.@"return", "expect a `return` keyword.");
    const expr = try self.expression();
    _ = try self.consume(.semicolon, "Expect a ';' ending a return statement.");
    return .{ .keyword = token, .expr = expr };
}

fn expression(self: *Parser) Error!AST.Expression {
    return switch (self.peek().value) {
        .constant => .{ .constant = .{ .tok = try self.consume(.constant, "Expect a constant expression.") } },
        .tilde, .hyphen => .{ .unary = try self.unary_expr() },
        .left_paren => try self.group_expr(),
        else => Error.unexpected_token,
    };
}

fn group_expr(self: *Parser) Error!AST.Expression {
    _ = try self.consume(.left_paren, "expect '(' before group expr.");
    const expr = try self.expression();
    _ = try self.consume(.right_paren, "expect ')' after group expr.");
    return expr;
}

fn unary_expr(self: *Parser) Error!AST.Unary {
    return .{
        .operator = self.advance(),
        .expr = (try self.expression()).create(self.allocator) catch return Error.memory_error,
    };
}

fn consume(self: *Parser, kind: TokenKind, msg: []const u8) Error!Token {
    try self.assert_has_more_tokens();

    const cur = self.peek();
    if (cur.value != kind) {
        stderr.print("is {}, {s}\n", .{ cur, msg }) catch return Error.writer_error;
        return Error.unexpected_token;
    }

    return self.advance();
}

fn is_at_end(self: *Parser) bool {
    return self.index >= self.tokens.len;
}

fn assert_has_more_tokens(self: *Parser) Error!void {
    if (self.is_at_end()) return Error.no_more_tokens;
}

fn advance(self: *Parser) Token {
    const cur = self.peek();
    self.index += 1;
    return cur;
}

fn peek(self: *Parser) Token {
    return self.tokens[self.index];
}
