const std = @import("std");
const cz = @import("root.zig");
const Lexer = cz.Lexer;
const Parser = cz.Parser;
const AcceptParentFn = @import("meta.zig").AcceptParentFn;
const IR = cz.TackyIR;

pub fn generate(program: IR.Program, allocator: std.mem.Allocator) anyerror!AssemblyProgram {
    const generator = try AssemblyGenerator.init(allocator);
    defer generator.deinit();

    const asm_program_pass1 = try generator.gen(program);
    defer asm_program_pass1.deinit();

    const pseudo_eliminator = try PseudoRegistersEliminator.init(allocator);
    defer pseudo_eliminator.deinit();

    const asm_program_pass2 = try pseudo_eliminator.transform(asm_program_pass1);
    defer asm_program_pass2.deinit();

    const fixer = try FixUpInstructionsPass.init(allocator);
    defer fixer.deinit();

    const asm_program_pass3 = try fixer.transform(asm_program_pass2);

    return asm_program_pass3;
}

pub const AssemblyProgram = struct {
    function_definition: Function,
    arena: std.heap.ArenaAllocator,

    pub fn deinit(self: AssemblyProgram) void {
        self.arena.deinit();
    }

    pub fn emit(self: AssemblyProgram, writer: std.io.AnyWriter) !void {
        const emitter = Emitter{ .writer = writer };
        try emitter.emit(self);
    }
};

pub const Function = struct {
    name: []const u8,
    instructions: InstructionsArray,
    allocated_stack_size: u32 = 0,
};

pub const InstructionsArray = std.ArrayList(Instruction);

pub const Instruction = union(enum) {
    mov: Move,
    unary: Unary,
    alloc_stack: AllocateStack,
    ret: Return,

    pub const accept = AcceptParentFn(@This());
};

pub const Return = void;

pub const AllocateStack = struct {
    amount: u32,
};

pub const UnaryOperator = enum {
    neg,
    not,
};

pub const Unary = struct {
    operator: UnaryOperator,
    operand: Operand,
};

pub const Move = struct {
    src: Operand,
    dst: Operand,
};

pub const Operand = union(enum) {
    immediate: Immediate,
    register: Register,
    pseudo: Pseudo,
    stack: Stack,

    pub const accept = AcceptParentFn(@This());
};

pub const Stack = struct {
    int: u32,
};

pub const Pseudo = struct {
    id: []const u8,
};

pub const Immediate = struct {
    value: []const u8,
};

pub const Register = union(enum) {
    byte_low: []const u8,
    byte_high: []const u8,
    short: []const u8,
    word: []const u8,
    quadword: []const u8,

    pub fn name(self: Register) []const u8 {
        return switch (self) {
            inline else => |str| str,
        };
    }
};

pub const Error = anyerror;

pub const AssemblyGenerator = @This();

allocator: std.mem.Allocator,
instructions: ?*InstructionsArray = null,

pub fn init(allocator: std.mem.Allocator) !*AssemblyGenerator {
    const this = try allocator.create(AssemblyGenerator);
    this.* = .{ .allocator = allocator };
    return this;
}

pub fn deinit(self: *AssemblyGenerator) void {
    self.allocator.destroy(self);
}

pub fn gen(self: *AssemblyGenerator, program: IR.Program) Error!AssemblyProgram {
    var gen_asm: AssemblyProgram = undefined;
    gen_asm.arena = .init(self.allocator);

    const prev_allocator = self.allocator;
    self.allocator = gen_asm.arena.allocator();
    defer self.allocator = prev_allocator;

    gen_asm.function_definition = try self.accept_function(program.function_definition);

    return gen_asm;
}

pub fn accept_function(self: *AssemblyGenerator, fun: IR.Function) Error!Function {
    return .{
        .name = fun.identifier.value.identifier,
        .instructions = try self.accept_instructions(fun.body),
    };
}

pub fn accept_instructions(self: *AssemblyGenerator, insts: []IR.Instruction) Error!InstructionsArray {
    var array = InstructionsArray.init(self.allocator);
    const prev = self.instructions;
    self.instructions = &array;
    defer self.instructions = prev;

    for (insts) |inst| {
        try inst.accept(self, void);
    }

    return array;
}

pub fn accept_return(self: *AssemblyGenerator, ret: IR.Return) Error!void {
    try self.instructions.?.appendSlice(&.{
        .{ .mov = .{ .src = try ret.value.accept(self, Operand), .dst = .{ .register = .{ .word = "eax" } } } },
        .ret,
    });
}

pub fn accept_unary(self: *AssemblyGenerator, unary: IR.Unary) Error!void {
    const operator: UnaryOperator = switch (unary.operator) {
        .complement => .not,
        .negate => .neg,
    };

    const src = try unary.src.accept(self, Operand);
    const dst = try unary.dst.accept(self, Operand);

    try self.instructions.?.appendSlice(&.{
        .{ .mov = .{
            .src = src,
            .dst = dst,
        } },
        .{ .unary = .{
            .operator = operator,
            .operand = dst,
        } },
    });
}

pub fn accept_variable(self: *AssemblyGenerator, variable: IR.Variable) Error!Operand {
    _ = self;
    return .{ .pseudo = .{ .id = variable.name } };
}

pub fn accept_constant(self: *AssemblyGenerator, constant: IR.Constant) Error!Operand {
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

    pub fn accept_function_def(self: PrettyPrinter, fun: Function) !void {
        try self.writer.print("FunctionDefinition(", .{});
        try self.accept_instructions(fun.instructions);
        try self.writer.print(")", .{});
    }

    pub fn accept_ret(self: PrettyPrinter, ret: Return) !void {
        _ = ret;
        try self.writer.print("ret", .{});
    }

    pub fn accept_alloc_stack(self: PrettyPrinter, alloc: AllocateStack) !void {
        try self.writer.print("AllocateStack({})", .{alloc.amount});
    }

    pub fn accept_unary(self: PrettyPrinter, unary: Unary) !void {
        try self.writer.print("Unary({s}, ", .{@tagName(unary.operator)});
        try unary.operand.accept(self, void);
        try self.writer.print(")", .{});
    }

    pub fn accept_mov(self: PrettyPrinter, mov: Move) !void {
        try self.writer.print("move(", .{});
        try mov.src.accept(self, void);
        try self.writer.print(", ", .{});
        try mov.dst.accept(self, void);
        try self.writer.print(")", .{});
    }

    pub fn accept_instructions(self: PrettyPrinter, instrs: InstructionsArray) !void {
        try self.writer.writeByte('[');
        for (instrs.items, 0..) |instr, idx| {
            try instr.accept(self, void);
            if (idx + 1 < instrs.items.len)
                try self.writer.print(", ", .{});
        }
        try self.writer.writeByte(']');
    }

    pub fn accept_stack(self: PrettyPrinter, stack: Stack) !void {
        try self.writer.print("Stack({})", .{stack.int});
    }

    pub fn accept_pseudo(self: PrettyPrinter, pseudo: Pseudo) !void {
        try self.writer.print("Pseudo({s})", .{pseudo.id});
    }

    pub fn accept_register(self: PrettyPrinter, reg: Register) !void {
        try self.writer.print("Register({s})", .{reg.name()});
    }

    pub fn accept_immediate(self: PrettyPrinter, imm: Immediate) !void {
        try self.writer.print("Immediate({s})", .{imm.value});
    }
};

pub const PseudoRegistersEliminator = struct {
    allocator: std.mem.Allocator,
    variables_offset: std.StringHashMap(u32),
    stack_offset: u32 = 0,

    pub fn init(allocator: std.mem.Allocator) !*PseudoRegistersEliminator {
        const self = try allocator.create(PseudoRegistersEliminator);
        self.* = .{
            .allocator = allocator,
            .variables_offset = .init(allocator),
        };
        return self;
    }

    pub fn deinit(self: *PseudoRegistersEliminator) void {
        self.variables_offset.deinit();
        self.allocator.destroy(self);
    }

    pub fn transform(self: *PseudoRegistersEliminator, program: AssemblyProgram) !AssemblyProgram {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        const prev = self.allocator;
        self.allocator = arena.allocator();
        defer self.allocator = prev;

        return .{
            .function_definition = try self.accept_function(program.function_definition),
            .arena = arena,
        };
    }

    fn accept_function(self: *PseudoRegistersEliminator, fun: Function) !Function {
        const new_instrs = try self.accept_instructions(fun.instructions);
        defer {
            self.stack_offset = 0;
            self.variables_offset.clearAndFree();
        }

        return .{
            .name = fun.name,
            .instructions = new_instrs,
            .allocated_stack_size = self.stack_offset,
        };
    }

    fn accept_instructions(self: *PseudoRegistersEliminator, insts: InstructionsArray) !InstructionsArray {
        var new_array = InstructionsArray.init(self.allocator);

        for (insts.items) |inst| {
            try new_array.append(try inst.accept(self, Instruction));
        }

        return new_array;
    }

    pub fn accept_mov(self: *PseudoRegistersEliminator, mov: Move) !Instruction {
        return Instruction{ .mov = .{
            .src = try mov.src.accept(self, Operand),
            .dst = try mov.dst.accept(self, Operand),
        } };
    }

    pub fn accept_ret(self: *PseudoRegistersEliminator, ret: Return) !Instruction {
        _ = self;
        _ = ret;
        return .ret;
    }

    pub fn accept_unary(self: *PseudoRegistersEliminator, unary: Unary) !Instruction {
        return .{ .unary = .{
            .operator = unary.operator,
            .operand = try unary.operand.accept(self, Operand),
        } };
    }

    pub fn accept_alloc_stack(self: *PseudoRegistersEliminator, alloc_stack: AllocateStack) !Instruction {
        _ = self;
        return Instruction{ .alloc_stack = alloc_stack };
    }

    pub fn accept_immediate(self: *PseudoRegistersEliminator, imm: Immediate) !Operand {
        _ = self;
        return .{ .immediate = imm };
    }

    pub fn accept_register(self: *PseudoRegistersEliminator, reg: Register) !Operand {
        _ = self;
        return .{ .register = reg };
    }

    pub fn accept_pseudo(self: *PseudoRegistersEliminator, pseudo: Pseudo) !Operand {
        const offset: u32 = self.variables_offset.get(pseudo.id) orelse value: {
            self.stack_offset += 4;
            try self.variables_offset.put(pseudo.id, self.stack_offset);
            break :value self.stack_offset;
        };

        return .{ .stack = .{ .int = offset } };
    }

    pub fn accept_stack(self: *PseudoRegistersEliminator, stack: Stack) !Operand {
        _ = self;
        return Operand{ .stack = stack };
    }
};

pub const FixUpInstructionsPass = struct {
    allocator: std.mem.Allocator,
    instructions: ?*InstructionsArray = null,

    pub fn init(allocator: std.mem.Allocator) !*FixUpInstructionsPass {
        const self = try allocator.create(FixUpInstructionsPass);
        self.* = .{ .allocator = allocator };
        return self;
    }

    pub fn deinit(self: *FixUpInstructionsPass) void {
        self.allocator.destroy(self);
    }

    pub fn transform(self: *FixUpInstructionsPass, program: AssemblyProgram) !AssemblyProgram {
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        const prev = self.allocator;
        self.allocator = arena.allocator();
        defer self.allocator = prev;

        return .{
            .function_definition = try self.accept_function(program.function_definition),
            .arena = arena,
        };
    }

    fn accept_function(self: *FixUpInstructionsPass, fun: Function) !Function {
        var new_instrs = InstructionsArray.init(self.allocator);
        const prev = self.instructions;
        self.instructions = &new_instrs;
        defer self.instructions = prev;

        try new_instrs.append(Instruction{ .alloc_stack = .{ .amount = fun.allocated_stack_size } });
        try self.accept_instructions(fun.instructions);

        return .{
            .name = fun.name,
            .instructions = new_instrs,
            .allocated_stack_size = fun.allocated_stack_size,
        };
    }

    fn accept_instructions(self: *FixUpInstructionsPass, insts: InstructionsArray) !void {
        for (insts.items) |inst| {
            try inst.accept(self, void);
        }
    }

    pub fn accept_mov(self: *FixUpInstructionsPass, mov: Move) !void {
        if (mov.src == .stack and mov.dst == .stack) {
            const temp: Operand = .{ .register = .{ .word = "r10d" } };
            try self.instructions.?.appendSlice(&.{
                Instruction{ .mov = .{ .src = mov.src, .dst = temp } },
                Instruction{ .mov = .{ .src = temp, .dst = mov.dst } },
            });
        } else {
            try self.instructions.?.append(.{ .mov = mov });
        }
    }

    pub fn accept_ret(self: *FixUpInstructionsPass, ret: Return) !void {
        _ = ret;
        try self.instructions.?.append(.ret);
    }

    pub fn accept_unary(self: *FixUpInstructionsPass, unary: Unary) !void {
        try self.instructions.?.append(.{ .unary = unary });
    }

    pub fn accept_alloc_stack(self: *FixUpInstructionsPass, alloc_stack: AllocateStack) !void {
        try self.instructions.?.append(.{ .alloc_stack = alloc_stack });
    }

    pub fn accept_immediate(self: *FixUpInstructionsPass, imm: Immediate) !Operand {
        _ = self;
        return .{ .immediate = imm };
    }

    pub fn accept_register(self: *FixUpInstructionsPass, reg: Register) !Operand {
        _ = self;
        return .{ .register = reg };
    }

    pub fn accept_pseudo(self: *FixUpInstructionsPass, pseudo: Pseudo) !Operand {
        _ = self;
        return .{ .pseudo = pseudo };
    }

    pub fn accept_stack(self: *FixUpInstructionsPass, stack: Stack) !Operand {
        _ = self;
        return .{ .stack = stack };
    }
};

pub const Emitter = struct {
    writer: std.io.AnyWriter,

    fn emit(self: Emitter, program: AssemblyProgram) !void {
        try self.writer.writeAll(".section .text\n");
        try self.accept_function_def(program.function_definition);
        try self.writer.writeAll(".section .note.GNU-stack,\"\",@progbits\n");
    }

    fn accept_function_def(self: Emitter, fun: Function) !void {
        try self.writer.print(".global {s}\n", .{fun.name});
        try self.writer.print("{s}:\n", .{fun.name});
        try self.writer.writeAll(
            \\pushq %rbp
            \\movq %rsp, %rbp
            \\
        );
        try self.accept_instructions(fun.instructions);
    }

    pub fn accept_ret(self: Emitter, ret: Return) !void {
        _ = ret;
        try self.writer.writeAll(
            \\movq %rbp, %rsp
            \\popq %rbp
            \\ret
        );
    }

    pub fn accept_alloc_stack(self: Emitter, alloc: AllocateStack) !void {
        try self.writer.print("subq ${}, %rsp", .{alloc.amount});
    }

    pub fn accept_unary(self: Emitter, unary: Unary) !void {
        const instructions_name = switch (unary.operator) {
            .neg => "negl",
            .not => "notl",
        };

        try self.writer.print("{s} ", .{instructions_name});
        try unary.operand.accept(self, void);
    }

    pub fn accept_mov(self: Emitter, mov: Move) !void {
        try self.writer.writeAll("movl ");
        try mov.src.accept(self, void);
        try self.writer.writeAll(", ");
        try mov.dst.accept(self, void);
    }

    pub fn accept_instructions(self: Emitter, instrs: InstructionsArray) !void {
        for (instrs.items) |instr| {
            try instr.accept(self, void);
            try self.writer.writeByte('\n');
        }
    }

    pub fn accept_stack(self: Emitter, stack: Stack) !void {
        try self.writer.print("-{}(%rbp)", .{stack.int});
    }

    pub fn accept_register(self: Emitter, reg: Register) !void {
        try self.writer.print("%{s}", .{reg.name()});
    }

    pub fn accept_pseudo(self: Emitter, p: Pseudo) !void {
        _ = self;
        _ = p;
        return error.pseudo_not_allowed;
    }

    pub fn accept_immediate(self: Emitter, imm: Immediate) !void {
        try self.writer.print("${s}", .{imm.value});
    }
};
