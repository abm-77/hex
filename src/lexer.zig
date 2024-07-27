const std = @import("std");
const ascii = std.ascii;
const assert = std.debug.assert;
const print = std.debug.print;

const LexErrorType = error{
    UNTERMINATED_STRING,
    UNTERMINATED_CHARACTER_LITERAL,
    INVALID_CHARACTER_LITERAL,
    INVALID_ESCAPE_SEQUENCE,
};

const LexError = struct {
    ty: LexErrorType,
    location: SourceLocation,
    pub fn report(self: @This()) void {
        print("Lex Error at {s}:{}:{}: {}\n", .{
            self.location.source_name,
            self.location.line,
            self.location.col,
            self.ty,
        });
    }
};

const LexResult = struct {
    const Self = @This();
    const TokenArray = std.ArrayList(Token);
    const ErrorArray = std.ArrayList(LexError);

    tokens: TokenArray,
    errors: ErrorArray,

    pub fn init(allocator: std.mem.Allocator) LexResult {
        return .{
            .tokens = TokenArray.init(allocator),
            .errors = ErrorArray.init(allocator),
        };
    }
    pub fn ok(self: Self) bool {
        return self.errors.items.len == 0;
    }
    pub fn append_token(self: *Self, token: Token) !void {
        try self.tokens.append(token);
    }
    pub fn append_error(self: *Self, err: LexError) !void {
        try self.errors.append(err);
    }
    pub fn deinit(self: *Self) void {
        self.tokens.deinit();
        self.errors.deinit();
    }
    pub fn list(self: *Self) void {
        for (self.tokens.items) |token| {
            print("Token: {s}\ntype: {}\n", .{ token.lexeme, token.ty });
            token.span.display();
            print("\n\n", .{});
        }
    }
};

const TokenType = enum {
    // general tokens
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_LEFT_BRACKET,
    TOKEN_RIGHT_BRACKET,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_SEMICOLON,
    TOKEN_COLON,
    TOKEN_MINUS,
    TOKEN_MINUS_EQUAL,
    TOKEN_PLUS,
    TOKEN_PLUS_EQUAL,
    TOKEN_SLASH,
    TOKEN_SLASH_EQUAL,
    TOKEN_STAR,
    TOKEN_STAR_EQUAL,
    TOKEN_PERCENT,
    TOKEN_PERCENT_EQUAL,
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,

    // literals
    TOKEN_IDENTIFIER,
    TOKEN_STRING_LITERAL,
    TOKEN_CHAR_LITERAL,
    TOKEN_INT_LITERAL,

    // keywords
    TOKEN_STRUCT,
    TOKEN_FUNC,
    TOKEN_CTRL_FOR,
    TOKEN_CTRL_WHILE,
    TOKEN_CTRL_BREAK,
    TOKEN_CTRL_CONTINUE,
    TOKEN_COND_IF,
    TOKEN_COND_ELSE,
    TOKEN_BOOL_OR,
    TOKEN_BOOL_AND,
    TOKEN_BOOL_TRUE,
    TOKEN_BOOL_FALSE,
    TOKEN_RETURN,

    TOKEN_COMMENT,
    TOKEN_SPACE,
    TOKEN_EOF,
};

const KEYWORDS = std.StaticStringMap(TokenType).initComptime(.{
    .{ "struct", .TOKEN_STRUCT },
    .{ "func", .TOKEN_FUNC },
    .{ "for", .TOKEN_CTRL_FOR },
    .{ "while", .TOKEN_CTRL_WHILE },
    .{ "break", .TOKEN_CTRL_BREAK },
    .{ "continue", .TOKEN_CTRL_CONTINUE },
    .{ "if", .TOKEN_COND_IF },
    .{ "else", .TOKEN_COND_ELSE },
    .{ "or", .TOKEN_BOOL_OR },
    .{ "and", .TOKEN_BOOL_AND },
    .{ "true", .TOKEN_BOOL_TRUE },
    .{ "false", .TOKEN_BOOL_FALSE },
    .{ "return", .TOKEN_RETURN },
});

const SourceLocation = struct {
    source_name: []const u8,
    byte_offset: u32,
    line: u32,
    col: u32,
};

const SourceSpan = struct {
    start: SourceLocation,
    end: SourceLocation,

    pub fn display(self: @This()) void {
        print("start: line: {}, col: {}, offset: {}\n", .{ self.start.line, self.start.col, self.start.byte_offset });
        print("end: line: {}, col: {}, offset: {}\n", .{ self.end.line, self.end.col, self.end.byte_offset });
    }
};

const Token = struct {
    ty: TokenType,
    lexeme: []const u8,
    span: SourceSpan,
};

pub const Lexer = struct {
    const Self = @This();

    source: []const u8,
    allocator: std.mem.Allocator,
    start_pos: SourceLocation,
    curr_pos: SourceLocation,

    fn eof(self: *Self) bool {
        return self.curr_pos.byte_offset >= self.source.len;
    }

    fn peek(self: *Self) u8 {
        return self.source[self.curr_pos.byte_offset];
    }

    fn peek_is(self: *Self, expected: u8) bool {
        return self.source[self.curr_pos.byte_offset] == expected;
    }

    fn peek_digit(self: *Self) bool {
        return ascii.isDigit(self.source[self.curr_pos.byte_offset]);
    }

    fn peek_hex(self: *Self) bool {
        return ascii.isHex(self.source[self.curr_pos.byte_offset]);
    }

    fn peek_alphanum(self: *Self) bool {
        return ascii.isAlphanumeric(self.source[self.curr_pos.byte_offset]);
    }

    fn next_character(self: *Self) ?u8 {
        if (self.eof()) return null;

        const char = self.source[self.curr_pos.byte_offset];
        if (char == '\n') {
            self.curr_pos.line += 1;
            self.curr_pos.col = 1;
        } else {
            self.curr_pos.col += 1;
        }
        self.curr_pos.byte_offset += 1;

        return char;
    }

    fn make_token(self: *Self, ty: TokenType) Token {
        return Token{
            .ty = ty,
            .span = SourceSpan{
                .start = self.start_pos,
                .end = self.curr_pos,
            },
            .lexeme = self.source[self.start_pos.byte_offset..self.curr_pos.byte_offset],
        };
    }

    fn advance(self: *Self) void {
        _ = self.next_character();
    }

    fn peek_advance_if(self: *Self, expected: u8) bool {
        if (self.peek() == expected) {
            self.advance();
            return true;
        } else {
            return false;
        }
    }

    fn make_error(self: *Self, ty: LexErrorType) LexError {
        return .{
            .ty = ty,
            .location = self.start_pos,
        };
    }

    fn make_comment(self: *Self) Token {
        while (!self.peek_is('\n') and !self.eof()) self.advance();
        return self.make_token(.TOKEN_COMMENT);
    }

    fn make_char(self: *Self) !Token {
        const c = self.next_character();
        if (c == null) return error.INVALID_CHARACTER_LITERAL;

        const char = c.?;
        if (ascii.isWhitespace(char) and char != ' ') {
            return error.INVALID_CHARACTER_LITERAL;
        }
        switch (char) {
            '\\' => {
                const esc = self.next_character();
                if (esc) |escape| {
                    switch (escape) {
                        'n', 't', '\'', '\"', '\\' => {
                            if (!self.peek_is('\'')) return error.UNTERMINATED_CHARACTER_LITERAL;
                            _ = self.next_character();
                            return self.make_token(.TOKEN_CHAR_LITERAL);
                        },
                        else => {},
                    }
                }
                return error.INVALID_ESCAPE_SEQUENCE;
            },
            '\'', '\"' => return error.INVALID_CHARACTER_LITERAL,
            else => {
                if (!self.peek_is('\'')) {
                    self.advance();
                    return error.UNTERMINATED_CHARACTER_LITERAL;
                } else {
                    self.advance();
                    return self.make_token(.TOKEN_CHAR_LITERAL);
                }
            },
        }
    }

    fn make_string(self: *Self) !Token {
        var terminated = false;
        var char: ?u8 = self.next_character();
        while (true) : (char = self.next_character()) {
            if (char == null or char.? == '\n') break;
            if (char.? == '\"') {
                terminated = true;
                break;
            }
        }
        if (!terminated) return error.UNTERMINATED_STRING;
        return self.make_token(.TOKEN_STRING_LITERAL);
    }

    fn make_number(self: *Self) Token {
        const hex = self.peek_advance_if('x');
        if (hex) {
            self.advance();
            while (self.peek_hex()) self.advance();
        } else {
            // TODO: parse floats
            while (self.peek_digit()) self.advance();
        }
        return self.make_token(.TOKEN_INT_LITERAL);
    }

    fn make_identifier(self: *Self) Token {
        while (self.peek_alphanum() and !self.eof()) self.advance();
        const lexeme = self.source[self.start_pos.byte_offset..self.curr_pos.byte_offset];
        if (KEYWORDS.get(lexeme)) |keyword| return self.make_token(keyword);
        return self.make_token(.TOKEN_IDENTIFIER);
    }

    fn make_token_if_else(self: *Self, expected: u8, on_match: TokenType, otherwise: TokenType) Token {
        const matched = self.peek_advance_if(expected);
        return if (matched) self.make_token(on_match) else self.make_token(otherwise);
    }

    fn consume_token(self: *Self) !Token {
        self.start_pos = self.curr_pos;
        const c = self.next_character();
        if (c == null) return self.make_token(.TOKEN_EOF);

        const char = c.?;
        if (ascii.isWhitespace(char)) return self.make_token(.TOKEN_SPACE);
        if (ascii.isAlphabetic(char) or char == '_') return self.make_identifier();
        if (ascii.isDigit(char)) return self.make_number();
        return switch (char) {
            '\"' => self.make_string(),
            '\'' => self.make_char(),
            '(' => self.make_token(.TOKEN_LEFT_PAREN),
            ')' => self.make_token(.TOKEN_RIGHT_PAREN),
            '{' => self.make_token(.TOKEN_LEFT_BRACE),
            '}' => self.make_token(.TOKEN_RIGHT_BRACE),
            '[' => self.make_token(.TOKEN_LEFT_BRACKET),
            ']' => self.make_token(.TOKEN_RIGHT_BRACKET),
            ';' => self.make_token(.TOKEN_SEMICOLON),
            ':' => self.make_token(.TOKEN_COLON),
            ',' => self.make_token(.TOKEN_COMMA),
            '.' => self.make_token(.TOKEN_DOT),
            '#' => self.make_comment(),
            '+' => self.make_token_if_else('=', .TOKEN_PLUS_EQUAL, .TOKEN_PLUS),
            '-' => self.make_token_if_else('=', .TOKEN_MINUS_EQUAL, .TOKEN_MINUS),
            '*' => self.make_token_if_else('=', .TOKEN_STAR_EQUAL, .TOKEN_STAR),
            '/' => self.make_token_if_else('=', .TOKEN_SLASH_EQUAL, .TOKEN_SLASH),
            '%' => self.make_token_if_else('=', .TOKEN_PERCENT_EQUAL, .TOKEN_PERCENT),
            '!' => self.make_token_if_else('=', .TOKEN_BANG_EQUAL, .TOKEN_BANG),
            '=' => self.make_token_if_else('=', .TOKEN_EQUAL_EQUAL, .TOKEN_EQUAL),
            '<' => self.make_token_if_else('=', .TOKEN_LESS_EQUAL, .TOKEN_LESS),
            '>' => self.make_token_if_else('=', .TOKEN_GREATER_EQUAL, .TOKEN_GREATER),
            else => std.debug.panic("invalid token encountered: {c}", .{char}),
        };
    }

    pub fn lex(self: *Self) !LexResult {
        var result = LexResult.init(self.allocator);
        while (!self.eof()) {
            if (self.consume_token()) |token| {
                switch (token.ty) {
                    .TOKEN_SPACE, .TOKEN_COMMENT => {},
                    else => try result.append_token(token),
                }
            } else |err| {
                try result.append_error(self.make_error(err));
            }
        }
        return result;
    }

    pub fn init(allocator: std.mem.Allocator, source_name: []const u8, source: []const u8) Self {
        return Lexer{
            .source = source,
            .allocator = allocator,
            .start_pos = SourceLocation{
                .source_name = source_name,
                .col = 1,
                .line = 1,
                .byte_offset = 0,
            },
            .curr_pos = SourceLocation{
                .source_name = source_name,
                .col = 1,
                .line = 1,
                .byte_offset = 0,
            },
        };
    }
};
