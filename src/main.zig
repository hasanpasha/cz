const std = @import("std");
const exit = std.process.exit;
const cz = @import("cz");
const Lexer = cz.Lexer;
const Parser = cz.Parser;
const TackyIRGenerator = cz.TackyIRGenerator;
const asm_generators = cz.asm_generators;

const clap = @import("clap");

const stderr = std.io.getStdErr().writer();
const stdout = std.io.getStdOut().writer();

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
    defer if (gpa.deinit() == .leak) @panic("memory leak");
    const allocator = gpa.allocator();
    // const allocator = std.heap.page_allocator;

    const params = comptime clap.parseParamsComptime(
        \\-h, --help            Display this help and exit.
        \\-l, --lex             Run the lexer only.
        \\-p, --parse           Run the parser only.
        \\-t, --tacky           Run the TackyIR generator only.
        \\-c, --codegen         Run the assembly generator without emitting code.
        \\-v, --verbose         print generated stuff.
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

    const code = file.readToEndAllocOptions(allocator, 64_000, null, @alignOf(u8), 0) catch std.process.exit(1);
    defer allocator.free(code);

    const tokens = Lexer.lex(allocator, code, processed_path) catch |err| {
        stderr.print("failed lexing file: `{}`\n", .{err}) catch {};
        exit(2);
    };
    defer tokens.deinit();

    if (res.args.verbose != 0 and res.args.lex == 0) {
        for (tokens.items) |tok| {
            stdout.print("{}\n", .{tok}) catch break;
        }
    }

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

    if (res.args.verbose != 0 and res.args.parse == 0) {
        stdout.print("{p}\n", .{program_ast}) catch {};
    }

    if (res.args.parse != 0) {
        stdout.print("{p}\n", .{program_ast}) catch {};
        exit(0);
    }

    const tackyIR_gen = TackyIRGenerator.init(allocator) catch |err| {
        stderr.print("memory error while creating `TackyIRGenerator` instance: {}", .{err}) catch {};
        exit(4);
    };
    defer tackyIR_gen.deinit();

    const program_tackyIR = tackyIR_gen.gen(program_ast) catch |err| {
        stderr.print("error on TackyIR gen: {}\n", .{err}) catch {};
        exit(4);
    };
    defer program_tackyIR.deinit();

    if (res.args.verbose != 0 and res.args.tacky == 0) {
        stdout.print("{p}\n", .{program_tackyIR}) catch {};
    }

    if (res.args.tacky != 0) {
        stdout.print("{p}\n", .{program_tackyIR}) catch {};
        exit(0);
    }

    const x86_64_generator = try asm_generators.X86_64ASMGenerator.init(allocator);
    defer x86_64_generator.deinit();

    const generator = x86_64_generator.generator();
    const asm_program = try generator.generate(program_tackyIR);
    defer asm_program.destroy();

    if (res.args.verbose != 0 and res.args.codegen == 0) {
        stdout.print("{}\n", .{asm_program}) catch {};
    }

    if (res.args.codegen != 0) {
        stdout.print("{}\n", .{asm_program}) catch {};
        exit(0);
    }

    const asm_output_path = try replace_extension(allocator, input_path, ".c", ".s");
    defer allocator.free(asm_output_path);

    const asm_file = try std.fs.createFileAbsolute(asm_output_path, .{});
    defer {
        asm_file.close();
        std.fs.deleteFileAbsolute(asm_output_path) catch {};
    }

    asm_program.emit(asm_file.writer().any()) catch |err| {
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
