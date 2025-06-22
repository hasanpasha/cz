const std = @import("std");
const cz = @import("cz");

const stderr = std.io.getStdErr().writer();

pub const Location = struct {
    filename: ?[]const u8,
    line: usize,
    column: usize,

    pub fn start(filename: ?[]const u8) Location {
        return Location{ .filename = filename, .line = 1, .column = 1 };
    }

    pub fn format(self: Location, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{s}{s}{}:{}", .{
            self.filename orelse "",
            if (self.filename != null) ":" else "",
            self.line,
            self.column,
        });
    }
};

pub const TokenKind = enum {
    identifier,
    constant,
    int,
    void,
    @"return",
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    semicolon,
    tilde,
    hyphen,
    hyphen_hyphen,
    eoi,
};

pub const TokenValue = union(TokenKind) {
    identifier: []const u8,
    constant: []const u8,
    int,
    void,
    @"return",
    left_paren,
    right_paren,
    left_brace,
    right_brace,
    semicolon,
    tilde,
    hyphen,
    hyphen_hyphen,
    eoi,

    pub fn format(self: TokenValue, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .identifier => |lexeme| try writer.print("{s}(\"{s}\")", .{ @tagName(self), lexeme }),
            .constant => |lexeme| try writer.print("{s}({s})", .{ @tagName(self), lexeme }),
            else => try writer.print("{s}", .{@tagName(self)}),
        }
    }
};

pub const Token = struct {
    value: TokenValue,
    location: Location,

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        try writer.print("{}: {}", .{ self.location, self.value });
    }
};

fn is_one_of_u8(list: []const u8, val: u8) bool {
    for (list) |value| {
        if (val == value) return true;
    }
    return false;
}

pub fn is_one_of(T: type, list: []const T, val: T) bool {
    return switch (T) {
        u8 => is_one_of_u8(list, val),
        else => @compileError("unimpelmented type"),
    };
}

source: [:0]const u8,
start: [*:0]const u8,
current: [*:0]const u8,
end: [*:0]const u8,
location: Location,

alloc: std.mem.Allocator,

const Lexer = @This();

pub fn lex(alloc: std.mem.Allocator, source: [:0]const u8, filename: []const u8) !std.ArrayList(Token) {
    const lexer = try init(alloc, source, filename);
    defer lexer.deinit();

    var tokens = std.ArrayList(Token).init(alloc);

    while (true) {
        const tok = try lexer.next_token();
        try tokens.append(tok);
        if (tok.value == .eoi) break;
    }

    return tokens;
}

pub fn init(alloc: std.mem.Allocator, source: [:0]const u8, filename: []const u8) !*Lexer {
    const lexer = try alloc.create(Lexer);

    lexer.* = .{
        .source = source,
        .start = source.ptr,
        .current = source.ptr,
        .end = source.ptr + (source.len - 1),
        .location = .start(filename),
        .alloc = alloc,
    };

    return lexer;
}

pub fn deinit(self: *Lexer) void {
    self.alloc.destroy(self);
}

fn skip_comments(self: *Lexer) void {
    if (self.match_str("//")) {
        while (!self.is_at_end() and !self.match_char('\n')) {
            _ = self.advance();
        }
        self.skip_whitespaces();
    }

    if (self.match_str("/*")) {
        while (!self.is_at_end() and !self.match_str("*/")) {
            _ = self.advance();
        }
        self.skip_whitespaces();
    }
}

fn skip_whitespaces(self: *Lexer) void {
    while (!self.is_at_end() and std.ascii.isWhitespace(self.peek())) _ = self.advance();
    self.skip_comments();
}

fn advance(self: *Lexer) u8 {
    if (self.is_at_end()) return 0;
    const cur = self.peek();
    self.current += 1;

    if (cur == '\n') {
        self.location.line += 1;
        self.location.column = 1;
    } else {
        self.location.column += 1;
    }

    return cur;
}

fn peek(self: *Lexer) u8 {
    return self.current[0];
}

fn peek_next(self: *Lexer) u8 {
    return self.current[1];
}

fn is_at_end(self: *Lexer) bool {
    return self.current[0] == 0;
}

fn match_char(self: *Lexer, char: u8) bool {
    if (self.is_at_end()) return false;
    if (self.peek() == char) {
        _ = self.advance();
        return true;
    }
    return false;
}

fn match_str(self: *Lexer, needle: []const u8) bool {
    if ((self.end - self.current) < needle.len) return false;
    if (std.mem.eql(u8, needle, self.current[0..needle.len])) {
        for (0..needle.len) |_| {
            _ = self.advance();
        }
        return true;
    }
    return false;
}

fn current_lexeme_len(self: *Lexer) usize {
    return self.current - self.start;
}

fn current_lexeme(self: *Lexer) []const u8 {
    return self.start[0..self.current_lexeme_len()];
}

fn current_lexeme_location(self: *Lexer) Location {
    var this_loc: Location = self.location;
    this_loc.column -= self.current_lexeme_len();
    return this_loc;
}

fn token(self: *Lexer, comptime kind: TokenKind) Token {
    const value: TokenValue = switch (kind) {
        .identifier => .{ .identifier = self.current_lexeme() },
        .constant => .{ .constant = self.current_lexeme() },
        inline else => @unionInit(TokenValue, @tagName(kind), {}),
    };

    return Token{
        .value = value,
        .location = self.current_lexeme_location(),
    };
}

fn err(self: *Lexer, comptime fmt: []const u8, args: anytype) void {
    var buffer: [1024]u8 = undefined;
    const msg = std.fmt.bufPrint(&buffer, fmt, args) catch return;

    stderr.print("{}: {s}\n", .{ self.current_lexeme_location(), msg }) catch {};
}

pub fn next_token(self: *Lexer) !Token {
    self.skip_whitespaces();

    self.start = self.current;

    const char = self.advance();

    return switch (char) {
        0 => self.token(.eoi),
        '(' => self.token(.left_paren),
        ')' => self.token(.right_paren),
        '{' => self.token(.left_brace),
        '}' => self.token(.right_brace),
        ';' => self.token(.semicolon),
        '~' => self.token(.tilde),
        '-' => if (self.peek() == '-') self.token(.hyphen_hyphen) else self.token(.hyphen),
        'a'...'z', 'A'...'Z', '_' => word: {
            while ((!self.is_at_end() and std.ascii.isAlphanumeric(self.peek()) or self.peek() == '_')) _ = self.advance();

            const this_lexeme = self.current_lexeme();
            if (std.mem.eql(u8, this_lexeme, "int")) {
                break :word self.token(.int);
            } else if (std.mem.eql(u8, this_lexeme, "void")) {
                break :word self.token(.void);
            } else if (std.mem.eql(u8, this_lexeme, "return")) {
                break :word self.token(.@"return");
            }

            break :word self.token(.identifier);
        },
        '0'...'9' => constant: {
            while (!self.is_at_end() and std.ascii.isDigit(self.peek())) {
                _ = self.advance();
            }
            if (!self.is_at_end() and std.ascii.isAlphabetic(self.peek())) {
                self.err("encountered an alphabetic character `{c}` while lexing a number constant.", .{self.peek()});
                break :constant error.unrecognized;
            }
            break :constant self.token(.constant);
        },
        else => keyword: {
            const this_lexeme = self.current_lexeme();
            self.err("unrecognized lexeme: \"{s}\"", .{this_lexeme});
            break :keyword error.unrecognized;
        },
    };
}
