pub const std = @import("std");
pub const cz = @import("root.zig");
pub const AST = cz.AST;
pub const TackyIR = cz.TackyIR;

const InstructionsIRArray = std.ArrayList(TackyIR.Instruction);

const Error = anyerror;

const TackyIRGenerator = @This();

allocator: std.mem.Allocator,
instructions: ?*InstructionsIRArray = null,
tmp_count: u32 = 0,

pub fn init(allocator: std.mem.Allocator) !*TackyIRGenerator {
    const self = try allocator.create(TackyIRGenerator);
    self.* = .{ .allocator = allocator };
    return self;
}

pub fn deinit(self: *TackyIRGenerator) void {
    self.allocator.destroy(self);
}

pub fn gen(self: *TackyIRGenerator, program: AST.Program) Error!TackyIR.Program {
    var arena = std.heap.ArenaAllocator.init(self.allocator);
    const prev = self.allocator;
    self.allocator = arena.allocator();
    defer self.allocator = prev;

    const function = try self.accept_function(program.function);

    return .{ .function_definition = function, .arena = arena };
}

pub fn accept_function(self: *TackyIRGenerator, function: AST.Function) Error!TackyIR.Function {
    var array = InstructionsIRArray.init(self.allocator);
    const prev_array = self.instructions;
    self.instructions = &array;
    defer self.instructions = prev_array;

    try function.body.accept(self, void);
    return .{ .identifier = function.name, .body = try array.toOwnedSlice() };
}

pub fn accept_return(self: *TackyIRGenerator, ret: AST.Return) Error!void {
    const value = try ret.expr.accept(self, TackyIR.Value);
    try self.instructions.?.append(.{ .@"return" = .{ .value = value } });
}

pub fn accept_constant(self: *TackyIRGenerator, constant: AST.Constant) Error!TackyIR.Value {
    _ = self;
    return .{ .constant = .{ .tok = constant.tok } };
}

fn make_temp(self: *TackyIRGenerator) Error![]const u8 {
    const name = try std.fmt.allocPrint(self.allocator, "tmp.{}", .{self.tmp_count});
    self.tmp_count += 1;
    return name;
}

pub fn accept_unary(self: *TackyIRGenerator, unary: AST.Unary) Error!TackyIR.Value {
    const src = try unary.expr.accept(self, TackyIR.Value);
    const dst = TackyIR.Value{ .variable = .{ .name = try self.make_temp() } };
    const operator: TackyIR.UnaryOperator = switch (unary.operator.value) {
        .hyphen => .negate,
        .tilde => .complement,
        else => unreachable,
    };

    try self.instructions.?.append(.{ .unary = .{
        .operator = operator,
        .src = src,
        .dst = dst,
    } });

    return dst;
}
