const std = @import("std");
const exit = std.process.exit;
const cz = @import("cz");
const Lexer = cz.Lexer;
const Parser = cz.Parser;
const PrettyPrinter = Parser.PrettyPrinter;
const AssemblyGenerator = cz.AssemblyGenerator;
const GeneratedProgram = AssemblyGenerator.AssemblyProgram;
const AssemblyEmitter = cz.AssemblyEmitter;
const X86_64Emitter = cz.emitters.X86_64Emitter;

const clap = @import("clap");

const stderr = std.io.getStdErr().writer();
const stdout = std.io.getStdOut().writer();

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer if (gpa.deinit() == .leak) @panic("memory leak");
    const allocator = gpa.allocator();

    const params = comptime clap.parseParamsComptime(
        \\-h, --help            Display this help and exit.
        \\-l, --lex             Run the lexer only.
        \\-p, --parse           Run the parser only.
        \\-c, --codegen         Run the assembly generator without emitting code.
        \\<FILE>
    );

    const parsers = comptime .{
        .FILE = clap.parsers.string,
    };

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = allocator,
    }) catch |err| {
        diag.report(stderr, err) catch {};
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        clap.help(stderr, clap.Help, &params, .{}) catch {};
        return;
    }

    const input_path: []const u8 = res.positionals[0] orelse {
        stderr.writeAll("error: no input file.\n") catch {};
        exit(1);
    };

    if (!std.mem.endsWith(u8, input_path, ".c")) {
        stderr.print("unvalid input file.\n", .{}) catch {};
        exit(69);
    }

    const processed_path = try replace_extension(allocator, input_path, ".c", ".cc");
    defer allocator.free(processed_path);

    var expand_proc = std.process.Child.init(&.{ "gcc", "-E", "-P", input_path, "-o", processed_path }, allocator);
    const expand_cmd_result = expand_proc.spawnAndWait() catch |err| {
        stderr.print("error while expanding the macros: {}\n", .{err}) catch {};
        exit(69);
    };
    defer std.fs.deleteFileAbsolute(processed_path) catch {};

    if (expand_cmd_result != .Exited and expand_cmd_result.Exited != 0) {
        stderr.print("error exapnding: {s}", .{input_path}) catch {};
        exit(69);
    }

    const file = std.fs.openFileAbsolute(processed_path, .{}) catch |err| {
        stderr.print("failed to open file {s}: {}", .{ processed_path, err }) catch {};
        exit(1);
    };
    defer file.close();

    const code = file.readToEndAllocOptions(allocator, 64_000, null, .of(u8), 0) catch std.process.exit(1);
    defer allocator.free(code);

    const tokens = Lexer.lex(allocator, code, processed_path) catch |err| {
        stderr.print("failed lexing file: `{}`\n", .{err}) catch {};
        exit(2);
    };
    defer tokens.deinit();

    if (res.args.lex != 0) {
        for (tokens.items) |tok| {
            stdout.print("{}\n", .{tok}) catch exit(2);
        }
        exit(0);
    }

    const parser = Parser.init(allocator, tokens.items) catch |err| {
        stderr.print("failed initializing the parser: {}", .{err}) catch {};
        exit(3);
    };
    defer parser.deinit();

    const program_ast = parser.parse_program() catch |err| {
        stderr.print("error parsing program: {}", .{err}) catch {};
        exit(3);
    };
    defer program_ast.deinit();

    if (res.args.parse != 0) {
        const printer = PrettyPrinter{ .writer = stdout.any() };
        printer.print(program_ast) catch {};
        exit(0);
    }

    const asm_gen = AssemblyGenerator.init(allocator) catch |err| {
        stderr.print("memory error while creating `AssemblyGenerator` instance: {}", .{err}) catch {};
        exit(4);
    };
    defer asm_gen.deinit();

    const program_asm = asm_gen.gen(program_ast) catch |err| {
        stderr.print("error on asm gen: {}", .{err}) catch {};
        exit(4);
    };
    defer program_asm.deinit();

    if (res.args.codegen != 0) {
        const asm_printer = AssemblyGenerator.PrettyPrinter{ .writer = stdout.any() };
        asm_printer.print(program_asm) catch {};
        exit(0);
    }

    const asm_output_path = try replace_extension(allocator, input_path, ".c", ".s");
    defer allocator.free(asm_output_path);

    std.log.info("out asm to: {s}", .{asm_output_path});

    const asm_file = try std.fs.createFileAbsolute(asm_output_path, .{});
    defer {
        asm_file.close();
        std.fs.deleteFileAbsolute(asm_output_path) catch {};
    }

    var x86_64 = X86_64Emitter{ .writer = asm_file.writer().any() };
    const emitter = x86_64.emitter();
    emitter.emit(program_asm) catch |err| {
        stderr.print("failed emitting assembly: {}\n", .{err}) catch {};
        exit(5);
    };

    const exe_output = try replace_extension(allocator, input_path, ".c", "");
    defer allocator.free(exe_output);

    var generate_exe_proc = std.process.Child.init(&.{ "gcc", "-o", exe_output, asm_output_path }, allocator);
    const term = generate_exe_proc.spawnAndWait() catch |err| {
        stderr.print("error compiling {s}: {}\n", .{ asm_output_path, err }) catch {};
        exit(5);
    };

    if (term != .Exited and term.Exited != 0) {
        stderr.print("gcc error compiling {s}: {}\n", .{ asm_output_path, term.Exited }) catch {};
        exit(5);
    }
}

fn replace_extension(alloc: std.mem.Allocator, input: []const u8, needle: []const u8, replacement: []const u8) ![]u8 {
    const buffer = try alloc.alloc(u8, input.len + replacement.len - needle.len);
    _ = std.mem.replace(u8, input, needle, replacement, buffer);

    return buffer;
}
