const std = @import("std");
const cz = @import("root.zig");
const Lexer = cz.Lexer;
const Parser = cz.Parser;
const meta = @import("meta.zig");
const AcceptParentFn = meta.AcceptParentFn;
const AST = cz.AST;

pub const AssemblyProgram = struct {
    function_definition: FunctionDefinition,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: AssemblyProgram) void {
        self.arena.deinit();
    }
};

pub const FunctionDefinition = struct {
    name: []const u8,
    instructions: std.ArrayList(Instruction),
};

pub const Instructions = std.ArrayList(Instruction);

pub const Instruction = union(enum) {
    mov: MoveInstruction,
    ret: ReturnInstruction,

    pub const accept = AcceptParentFn(@This());
};

pub const ReturnInstruction = void;

pub const MoveInstruction = struct {
    src: Operand,
    dst: Operand,
};

pub const Operand = union(enum) {
    immediate: Immediate,
    register: Register,

    pub const accept = AcceptParentFn(@This());
};

pub const Immediate = struct {
    value: []const u8,
};

pub const Register = struct {
    name: []const u8 = "eax",
};

pub const AssemblyGenerator = @This();

allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator) !*AssemblyGenerator {
    const this = try allocator.create(AssemblyGenerator);
    this.* = .{ .allocator = allocator };
    return this;
}

pub fn deinit(self: *AssemblyGenerator) void {
    self.allocator.destroy(self);
}

pub fn gen(self: *AssemblyGenerator, program: AST.Program) !AssemblyProgram {
    var gen_asm: AssemblyProgram = undefined;
    gen_asm.arena = .init(self.allocator);

    const prev_allocator = self.allocator;
    self.allocator = gen_asm.arena.allocator();
    defer self.allocator = prev_allocator;

    gen_asm.function_definition = try self.accept_function(program.function);

    return gen_asm;
}

pub fn accept_function(self: *AssemblyGenerator, fun: AST.Function) !FunctionDefinition {
    return .{
        .name = fun.name.value.identifier,
        .instructions = try fun.body.accept(self, Instructions),
    };
}

pub fn accept_return(self: *AssemblyGenerator, stmt: AST.Return) !Instructions {
    var array = Instructions.init(self.allocator);
    try array.append(.{ .mov = .{ .src = try stmt.expr.accept(self, Operand), .dst = .{ .register = .{} } } });
    try array.append(.ret);
    return array;
}

pub fn accept_unary(self: *AssemblyGenerator, unary: AST.Unary) !Operand {
    // try self.writer.print("Unary({}, ", .{unary.operator.value});
    // try unary.expr.accept(self, void);
    _ = self;
    _ = unary;
    return .{ .immediate = .{ .value = "NotImplemented" } };
}

pub fn accept_constant(self: *AssemblyGenerator, constant: AST.Constant) !Operand {
    _ = self;
    return .{ .immediate = .{ .value = constant.tok.value.constant } };
}

pub const PrettyPrinter = struct {
    writer: std.io.AnyWriter,

    pub fn print(self: PrettyPrinter, program: AssemblyProgram) !void {
        try self.writer.print("GeneratedProgram(", .{});
        try self.accept_function_def(program.function_definition);
        try self.writer.print(")\n", .{});
    }

    pub fn accept_function_def(self: PrettyPrinter, fun: FunctionDefinition) !void {
        try self.writer.print("FunctionDefinition(instructions=", .{});
        try self.accept_instructions(fun.instructions);
        try self.writer.print(")", .{});
    }

    pub fn accept_ret(self: PrettyPrinter, ret: ReturnInstruction) !void {
        _ = ret;
        try self.writer.print("ret", .{});
    }

    pub fn accept_mov(self: PrettyPrinter, mov: MoveInstruction) !void {
        try self.writer.print("move(src=", .{});
        try mov.src.accept(self, void);
        try self.writer.print(", dst=", .{});
        try mov.dst.accept(self, void);
        try self.writer.print(")", .{});
    }

    pub fn accept_instructions(self: PrettyPrinter, instrs: Instructions) !void {
        for (instrs.items, 0..) |instr, idx| {
            try instr.accept(self, void);
            if (idx + 1 < instrs.items.len)
                try self.writer.print(", ", .{});
        }
    }

    pub fn accept_register(self: PrettyPrinter, reg: Register) !void {
        try self.writer.print("Register({s})", .{reg.name});
    }

    pub fn accept_immediate(self: PrettyPrinter, imm: Immediate) !void {
        try self.writer.print("Immediate({s})", .{imm.value});
    }
};
