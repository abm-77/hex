const std = @import("std");
const lex = @import("lexer.zig");
const parse = @import("parser.zig");
const print = std.debug.print;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const file = "programs/program.inc";
    const source = try std.fs.cwd().readFileAlloc(allocator, file, std.math.maxInt(usize));
    defer allocator.free(source);

    var lexer = lex.Lexer.init(allocator, file, source);

    var lex_result = try lexer.lex();
    defer lex_result.deinit();

    if (!lex_result.ok()) {
        for (lex_result.errors.items) |err| {
            err.report();
        }
    }

    lex_result.list();

    var parser = parse.Parser.init(file, allocator, lex_result.tokens);
    var parse_result = try parser.parse();
    defer parse_result.deinit();

    for (parse_result.statements.items) |statement| {
        print("{}\n", .{statement});
    }
}
