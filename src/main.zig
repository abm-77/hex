const std = @import("std");
const lex = @import("lexer.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const source = try std.fs.cwd().readFileAlloc(allocator, "programs/test.inc", std.math.maxInt(usize));
    defer allocator.free(source);

    var lexer = lex.Lexer.init(allocator, "test.inc", source);

    var lex_result = try lexer.lex();
    defer lex_result.deinit();

    if (lex_result.ok()) {
        lex_result.list();
    } else {
        for (lex_result.errors.items) |err| {
            err.report();
        }
    }
}
