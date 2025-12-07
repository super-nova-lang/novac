const std = @import("std");
const novac = @import("novac");

const Lexer = novac.Lexer;
const Token = Lexer.Token;

pub fn main() !void {
    var gpa = std.heap.DebugAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const path = "tests/spec.nova";

    const cwd = std.fs.cwd();
    var file = try cwd.openFile(path, .{ .mode = .read_only });
    defer file.close();

    const stat = try file.stat();

    const buffer = try alloc.alloc(u8, stat.size);
    defer alloc.free(buffer);
    _ = try file.read(buffer);

    var lex = Lexer{
        .path = path,
        .content = buffer,
    };

    while (true) {
        const token = try lex.next();
        std.debug.print("Found: {any}\n", .{token});
        if (token == .Eof) break;
    }
}
