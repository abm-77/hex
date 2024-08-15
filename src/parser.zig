const std = @import("std");
const lex = @import("lexer.zig");

const Token = lex.Token;
const TokenType = lex.TokenType;
const TokenList = lex.TokenList;
const AstNodeList = std.ArrayList(*AstNode);
const ErrorList = std.ArrayList(ParseError);
const SymbolTypeTable = std.StringHashMap(Type);

const ParseError = error{
    OOM,
    UNEXPECTED_TOKEN,
};

pub const ParseResult = struct {
    const Self = @This();

    arena: std.heap.ArenaAllocator,
    statements: AstNodeList,
    errors: ErrorList,

    pub fn init(
        arena: std.heap.ArenaAllocator,
        statements: AstNodeList,
        errors: ErrorList,
    ) ParseResult {
        return .{
            .arena = arena,
            .statements = statements,
            .errors = errors,
        };
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }
};

const Precedence = enum(u32) {
    PRECEDENCE_LOWEST,
    PRECEDENCE_EQ, // ==, !=, >=, <=
    PRECEDENCE_LT_GT, // <, >
    PRECEDENCE_SUM, // +, -
    PRECEDENCE_PRODUCT, // *, /
    PRECEDENCE_PREFIX, // -, !
    PRECEDENCE_CALL, // func()
    PRECEDENCE_INDEX, // arr[]

    pub fn to_int(self: @This()) u32 {
        return @intFromEnum(self);
    }
};

// TODO: handle arrays of arrays
const Type = union(enum) {
    TYPE_VOID: void,
    TYPE_INT: void,
    TYPE_BOOL: void,
    TYPE_ARRAY: struct {
        subtype: *Type,
        len: usize,
    },
};

const AstNode = union(enum) {
    PROGRAM: struct {
        statements: AstNodeList,
    },
    EXPRESSION_STATEMENT: struct {
        expression: *AstNode,
    },
    VARIABLE_DEFINITION_STATEMENT: struct {
        ident: *AstNode, // IDENT_EXPRESSION
        ty: Type,
        value: *AstNode, // EXPRESSION
        is_const: bool,
    },
    VARIABLE_ASSIGN_STATEMENT: struct {
        ident: *AstNode, // IDENT_EXPRESSION
        value: *AstNode, // EXPRESSION
    },
    BLOCK_STATEMENT: struct {
        statements: AstNodeList,
    },
    RETURN_STATEMENT: struct {
        value: *AstNode, // EXPRESSION
    },
    IF_STATEMENT: struct {
        cond: *AstNode, // EXPRESSION
        cons: *AstNode, // BLOCK_STATEMENT
        alt: *AstNode, // BLOCK_STATEMENT
    },
    WHILE_STATEMENT: struct {
        cond: *AstNode, // EXPRESSION
        block: *AstNode, // BLOCK_STATEMENT
    },
    FOR_STATEMENT: struct {
        init: *AstNode, // EXPRESSION
        cond: *AstNode, // EXPRESSION
        cont: *AstNode, // EXPRESSION
        block: *AstNode, // BLOCK_STATEMENT
    },
    FUNCTION_DEFINITION_STATEMENT: struct {
        ident: *AstNode, // IDENT_EXPRESSION
        parameters: AstNodeList,
        block: *AstNode,
        return_type: Type,
    },
    IDENT_EXPRESSION: struct {
        name: []const u8,
    },
    INTEGER_LITERAL_EXPRESSION: struct {
        value: i64,
    },
    BOOLEAN_LITERAL_EXPRESSION: struct {
        value: bool,
    },
    STRING_LITERAL_EXPRESSION: struct {
        value: []const u8,
    },
    ARRAY_LITERAL_EXPRESSION: struct {
        len: usize,
        ty: Type,
        items: AstNodeList,
    },
    INDEX_EXPRESSION: struct {
        arr: *AstNode, // IDENT_EXPRESSION
        index: *AstNode, // EXPRESSION (must evaluate to int)
    },
    PREFIX_EXPRESSION: struct {
        prefix: TokenType,
        right: *AstNode, // EXPRESSION
    },
    INFIX_EXPRESSION: struct {
        left: *AstNode, // EXPRESSION
        operator: TokenType,
        right: *AstNode, // EXPRESSION
    },
    FUNCTION_CALL_EXPRESSION: struct {
        ident: *AstNode, // IDENT_EXPRESSION
        arguments: AstNodeList,
        return_type: Type,
    },
    FUNCTION_PARAMETER: struct {
        ident: *AstNode, // IDENT_EXPRESSION
        ty: Type, // IDENT_EXPRESSION
    },
};

pub const Parser = struct {
    const Self = @This();
    source_name: []const u8,
    arena: std.heap.ArenaAllocator,
    tokens: TokenList,
    token_idx: u32,
    symbol_type_table: SymbolTypeTable,

    fn unexpected_token(expected: TokenType, actual: TokenType) noreturn {
        return std.debug.panic("Expected token to be {}, but was actually {}", .{ expected, actual });
    }

    fn oom() noreturn {
        std.debug.panic("ran out of memory while allocating", .{});
    }

    pub fn init(source_name: []const u8, allocator: std.mem.Allocator, tokens: TokenList) Parser {
        const arena = std.heap.ArenaAllocator.init(allocator);
        return .{
            .source_name = source_name,
            .token_idx = 0,
            .arena = arena,
            .tokens = tokens,
            .symbol_type_table = undefined,
        };
    }

    inline fn peek(self: *Self) TokenType {
        return self.tokens.items[self.token_idx].ty;
    }

    inline fn match_token(self: *Self, ty: TokenType) bool {
        return self.peek() == ty;
    }

    fn next_token(self: *Self) Token {
        const token = self.tokens.items[self.token_idx];
        self.token_idx += 1;
        return token;
    }

    fn consume(self: *Self, ty: TokenType) Token {
        const token = self.next_token();
        if (token.ty == ty) {
            return token;
        } else unexpected_token(ty, token.ty);
        return undefined;
    }

    fn consume_array_type_denotation(self: *Self, subtype: Type) Type {
        self.advance_assert(.TOKEN_LEFT_BRACKET);
        const len = self.parse_integer_literal_expression();
        self.advance_assert(.TOKEN_RIGHT_BRACKET);
        return Type{
            .TYPE_ARRAY = .{
                .subtype = @constCast(&subtype),
                .len = @intCast(len.INTEGER_LITERAL_EXPRESSION.value),
            },
        };
    }

    fn single_or_array_type(self: *Self, ty: Type) Type {
        if (self.match_token(.TOKEN_LEFT_BRACKET)) {
            return self.consume_array_type_denotation(Type.TYPE_INT);
        } else {
            return ty;
        }
    }

    fn consume_type(self: *Self) Type {
        const token = self.next_token();
        const res: Type = switch (token.ty) {
            .TOKEN_VOID => Type.TYPE_VOID,
            .TOKEN_INT => self.single_or_array_type(Type.TYPE_INT),
            .TOKEN_BOOL => self.single_or_array_type(Type.TYPE_BOOL),
            else => unexpected_token(.TOKEN_TYPE, token.ty),
        };
        return res;
    }

    fn consume_ident(self: *Self) []const u8 {
        const token = self.next_token();
        switch (token.ty) {
            .TOKEN_IDENTIFIER => return token.lexeme,
            else => unexpected_token(.TOKEN_IDENTIFIER, token.ty),
        }
        return "";
    }

    fn consume_string_literal(self: *Self) []const u8 {
        const token = self.next_token();
        switch (token.ty) {
            .TOKEN_STRING_LITERAL => return token.lexeme,
            else => unexpected_token(.TOKEN_STRING_LITERAL, token.ty),
        }
        return "";
    }

    fn consume_int_literal(self: *Self) i32 {
        const int_lit = self.consume(.TOKEN_INT_LITERAL);
        return std.fmt.parseInt(i32, int_lit.lexeme, 10) catch {
            std.debug.panic("could not parse int literal", .{});
        };
    }

    fn consume_bool_literal(self: *Self) bool {
        const token = self.next_token();
        const res: bool = switch (token.ty) {
            .TOKEN_BOOL_TRUE => true,
            .TOKEN_BOOL_FALSE => false,
            else => std.debug.panic("invalid boolean literal", .{}),
        };
        return res;
    }

    inline fn advance_assert(self: *Self, ty: TokenType) void {
        _ = self.consume(ty);
    }

    pub fn parse(self: *Self) !ParseResult {
        const errors = ErrorList.init(self.arena.allocator());
        self.symbol_type_table = SymbolTypeTable.init(self.arena.allocator());
        var statements = AstNodeList.init(self.arena.allocator());
        while (!self.match_token(.TOKEN_EOF)) {
            try statements.append(self.parse_statement());
        }
        return ParseResult.init(self.arena, statements, errors);
    }

    fn parse_type(self: *Self) Type {
        return self.consume_type();
    }

    fn parse_ident_expression(self: *Self) *AstNode {
        return self.create_node(AstNode{ .IDENT_EXPRESSION = .{
            .name = self.consume_ident(),
        } });
    }

    fn parse_string_literal_expression(self: *Self) *AstNode {
        return self.create_node(AstNode{ .STRING_LITERAL_EXPRESSION = .{
            .value = self.consume_string_literal(),
        } });
    }

    fn parse_integer_literal_expression(self: *Self) *AstNode {
        return self.create_node(AstNode{ .INTEGER_LITERAL_EXPRESSION = .{
            .value = self.consume_int_literal(),
        } });
    }

    fn parse_boolean_literal_expression(self: *Self) *AstNode {
        return self.create_node(AstNode{ .BOOLEAN_LITERAL_EXPRESSION = .{
            .value = self.consume_bool_literal(),
        } });
    }

    fn deduce_type(self: *Self, expr: *AstNode) ?Type {
        return switch (expr.*) {
            AstNode.INTEGER_LITERAL_EXPRESSION => Type.TYPE_INT,
            AstNode.BOOLEAN_LITERAL_EXPRESSION => Type.TYPE_BOOL,
            AstNode.ARRAY_LITERAL_EXPRESSION => |*arr| Type{ .TYPE_ARRAY = .{
                .len = arr.len,
                .subtype = &arr.ty,
            } },
            AstNode.FUNCTION_CALL_EXPRESSION => |fc| fc.return_type,
            AstNode.INDEX_EXPRESSION => |idx| self.symbol_type_table.get(idx.arr.IDENT_EXPRESSION.name).?.TYPE_ARRAY.subtype.*,
            else => null,
        };
    }

    fn parse_array_literal_expression(self: *Self) *AstNode {
        const items = self.parse_expression_list(.TOKEN_LEFT_BRACKET, .TOKEN_RIGHT_BRACKET);
        return self.create_node(AstNode{
            .ARRAY_LITERAL_EXPRESSION = .{
                .ty = self.deduce_type(items.items[0]).?, // must be able to deduce type of array literal
                .len = items.items.len,
                .items = items,
            },
        });
    }

    fn parse_prefix_expression(self: *Self) *AstNode {
        const prefix = self.peek();
        self.advance_assert(prefix);
        return self.create_node(AstNode{ .PREFIX_EXPRESSION = .{
            .prefix = prefix,
            .right = self.parse_expression(.PRECEDENCE_PREFIX),
        } });
    }

    pub inline fn infix_precedence(token_ty: TokenType) Precedence {
        return switch (token_ty) {
            .TOKEN_EQUAL, .TOKEN_PLUS_EQUAL, .TOKEN_MINUS_EQUAL, .TOKEN_EQUAL_EQUAL, .TOKEN_BANG_EQUAL, .TOKEN_GREATER_EQUAL, .TOKEN_LESS_EQUAL => .PRECEDENCE_EQ,
            .TOKEN_GREATER, .TOKEN_LESS => .PRECEDENCE_LT_GT,
            .TOKEN_PLUS, .TOKEN_MINUS => .PRECEDENCE_SUM,
            .TOKEN_SLASH, .TOKEN_STAR => .PRECEDENCE_PRODUCT,
            .TOKEN_LEFT_PAREN => .PRECEDENCE_CALL,
            .TOKEN_LEFT_BRACKET => .PRECEDENCE_INDEX,
            else => .PRECEDENCE_LOWEST,
        };
    }

    fn parse_infix_expression(self: *Self, left_expr: *AstNode) *AstNode {
        const op = self.peek();
        const precedence = infix_precedence(op);
        self.advance_assert(op);
        const right_expr = self.parse_expression(precedence);
        return self.create_node(AstNode{ .INFIX_EXPRESSION = .{
            .left = left_expr,
            .operator = op,
            .right = right_expr,
        } });
    }

    fn parse_grouped_expression(self: *Self) *AstNode {
        self.advance_assert(.TOKEN_LEFT_PAREN);
        const expr = self.parse_expression(.PRECEDENCE_LOWEST);
        self.advance_assert(.TOKEN_RIGHT_PAREN);
        return expr;
    }

    fn parse_function_call_expression(self: *Self, function: *AstNode) *AstNode {
        const args = self.parse_expression_list(.TOKEN_LEFT_PAREN, .TOKEN_RIGHT_PAREN);
        const return_type = self.symbol_type_table.get(function.IDENT_EXPRESSION.name);
        if (return_type == null) {
            std.debug.panic("function must be defined before it is called.", .{});
            return undefined;
        }
        return self.create_node(AstNode{ .FUNCTION_CALL_EXPRESSION = .{
            .ident = function,
            .return_type = return_type.?,
            .arguments = args,
        } });
    }

    fn parse_index_expression(self: *Self, arr: *AstNode) *AstNode {
        self.advance_assert(.TOKEN_LEFT_BRACKET);
        const expr = self.parse_expression(.PRECEDENCE_LOWEST);
        self.advance_assert(.TOKEN_RIGHT_BRACKET);
        return self.create_node(AstNode{ .INDEX_EXPRESSION = .{
            .arr = arr,
            .index = expr,
        } });
    }

    fn parse_expression(self: *Self, precedence: Precedence) *AstNode {
        // prefix
        var left_expr = switch (self.peek()) {
            .TOKEN_IDENTIFIER => self.parse_ident_expression(),
            .TOKEN_INT_LITERAL => self.parse_integer_literal_expression(),
            .TOKEN_STRING_LITERAL => self.parse_string_literal_expression(),
            .TOKEN_BOOL_TRUE, .TOKEN_BOOL_FALSE => self.parse_boolean_literal_expression(),
            .TOKEN_LEFT_BRACKET => self.parse_array_literal_expression(),
            .TOKEN_BANG, .TOKEN_MINUS => self.parse_prefix_expression(),
            .TOKEN_LEFT_PAREN => self.parse_grouped_expression(),
            else => undefined,
        };

        if (left_expr == undefined) {
            // error
        }

        // zig fmt: off
        // infix
        while (!self.match_token(.TOKEN_SEMICOLON) and precedence.to_int() < infix_precedence(self.peek()).to_int()) {
            const infix = switch (self.peek()) {
                .TOKEN_EQUAL, .TOKEN_PLUS_EQUAL, .TOKEN_MINUS_EQUAL, .TOKEN_PLUS, .TOKEN_MINUS, .TOKEN_SLASH, .TOKEN_STAR, .TOKEN_EQUAL_EQUAL, 
                .TOKEN_BANG_EQUAL, .TOKEN_GREATER_EQUAL, .TOKEN_LESS_EQUAL, .TOKEN_GREATER, .TOKEN_LESS => self.parse_infix_expression(left_expr),
                .TOKEN_LEFT_PAREN => self.parse_function_call_expression(left_expr),
                .TOKEN_LEFT_BRACKET => self.parse_index_expression(left_expr),
                else => undefined,
            };

            if (infix == undefined) return left_expr;

            left_expr = infix; 
        }

        return left_expr;
    }

    fn parse_expression_list(self: *Self, start: TokenType, end: TokenType) AstNodeList {
        var list = AstNodeList.init(self.arena.allocator());
        self.advance_assert(start);
        if (self.match_token(end)) {
            self.advance_assert(end);
        } else {
            list.append(self.parse_expression(.PRECEDENCE_LOWEST)) catch oom();
            while (self.match_token(.TOKEN_COMMA)) {
                self.advance_assert(.TOKEN_COMMA);
                list.append(self.parse_expression(.PRECEDENCE_LOWEST)) catch oom();
            }
            self.advance_assert(end);
        }
        return list;
    }

    fn parse_statement(self: *Self) *AstNode{
        switch (self.peek()) {
            .TOKEN_CONST, .TOKEN_VAR => return self.parse_variable_definition_statement(),
            .TOKEN_RETURN => return self.parse_return_statement(),
            .TOKEN_COND_IF => return self.parse_if_statement(),
            .TOKEN_CTRL_WHILE => return self.parse_while_statement(),
            .TOKEN_CTRL_FOR => return self.parse_for_statement(),
            .TOKEN_FUNC => return self.parse_function_definition_statement(),
             else => return self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(self: *Self) *AstNode {
        defer self.advance_assert(.TOKEN_SEMICOLON);
        const expr = self.parse_expression(.PRECEDENCE_LOWEST);
        return self.create_node(AstNode{ .EXPRESSION_STATEMENT = .{
            .expression = expr,
        } });
    }

    fn parse_variable_definition_statement(self: *Self) *AstNode {
        defer self.advance_assert(.TOKEN_SEMICOLON);
        const is_const = self.match_token(.TOKEN_CONST);
        if (is_const) self.advance_assert(.TOKEN_CONST) else self.advance_assert(.TOKEN_VAR);
        const ident = self.parse_ident_expression();

        var ty: ?Type = null;
        if (self.match_token(.TOKEN_COLON)) {
            self.advance_assert(.TOKEN_COLON);
            ty = self.parse_type();
        }

        self.advance_assert(.TOKEN_EQUAL);
        const expr = self.parse_expression(.PRECEDENCE_LOWEST);

        if (ty == null) {
            ty = self.deduce_type(expr);
            if (ty == null) {
                std.debug.panic("could not deduce type! Explicity type notation required!", .{});
            }
        }
        self.symbol_type_table.put(ident.IDENT_EXPRESSION.name, ty.?) catch oom();

        return self.create_node(AstNode{ .VARIABLE_DEFINITION_STATEMENT = .{
            .ident = ident,
            .ty = ty.?,
            .value = expr,
            .is_const = is_const,
        } });
    }
    
    // TODO: Allow declaration
    fn parse_variable_assign_statement(self: *Self) *AstNode {
        defer self.advance_assert(.TOKEN_SEMICOLON);
        const ident = self.parse_ident_expression();
        self.advance_assert(.TOKEN_EQUAL);
        const exp = self.parse_expression(.PRECEDENCE_LOWEST);
        return self.create_node(AstNode{ .VARIABLE_ASSIGN_STATEMENT = .{
            .ident = ident,
            .value = exp,
        } });
    }

    fn parse_return_statement(self: *Self) *AstNode {
        defer self.advance_assert(.TOKEN_SEMICOLON);
        self.advance_assert(.TOKEN_RETURN);
        const value = self.parse_expression(.PRECEDENCE_LOWEST);
        return self.create_node(AstNode{ .RETURN_STATEMENT = .{
            .value = value,
        } });
    }

    fn parse_if_statement(self: *Self) *AstNode {
        self.advance_assert(.TOKEN_COND_IF);
        self.advance_assert(.TOKEN_LEFT_PAREN);
        const cond = self.parse_expression(.PRECEDENCE_LOWEST);
        self.advance_assert(.TOKEN_RIGHT_PAREN);
        const consequence = self.parse_block_statement();

        var alternative: *AstNode = undefined;
        if (self.match_token(.TOKEN_COND_ELSE)) {
            self.advance_assert(.TOKEN_COND_ELSE);
            alternative = self.parse_block_statement();
        }

        return self.create_node(AstNode{
            .IF_STATEMENT = .{
                .cond= cond,
                .cons= consequence,
                .alt = alternative,
            },
        });
    }

    fn parse_while_statement(self: *Self) *AstNode {
        self.advance_assert(.TOKEN_CTRL_WHILE);
        self.advance_assert(.TOKEN_LEFT_PAREN);
        const cond= self.parse_expression(.PRECEDENCE_LOWEST);
        self.advance_assert(.TOKEN_RIGHT_PAREN);
        const block = self.parse_block_statement();
        return self.create_node(AstNode{
            .WHILE_STATEMENT = .{
                .cond= cond,
                .block = block,
            },
        });
    }

    fn parse_for_statement(self: *Self) *AstNode{
        self.advance_assert(.TOKEN_CTRL_FOR);
        self.advance_assert(.TOKEN_LEFT_PAREN);
        const init_expr = self.parse_statement();
        const cond_expr = self.parse_statement();
        const cont_expr = self.parse_expression(.PRECEDENCE_LOWEST);
        self.advance_assert(.TOKEN_RIGHT_PAREN);
        const block = self.parse_block_statement();
        return self.create_node(AstNode{
            .FOR_STATEMENT = .{
                .init = init_expr,
                .cond= cond_expr,
                .cont = cont_expr,
                .block = block,
            },
        });
    }


    fn parse_function_definition_statement(self: *Self) *AstNode {
        var params = AstNodeList.init(self.arena.allocator());

        self.advance_assert(.TOKEN_FUNC);

        const name = self.parse_ident_expression();

        self.advance_assert(.TOKEN_LEFT_PAREN);
        if (self.match_token(.TOKEN_RIGHT_PAREN)) {
            self.advance_assert(.TOKEN_RIGHT_PAREN);
        } else {
            var ident = self.parse_ident_expression();
            self.advance_assert(.TOKEN_COLON);
            var ty = self.parse_type();
            params.append(self.create_node(AstNode{
                .FUNCTION_PARAMETER = .{
                    .ident = ident,
                    .ty = ty,
                }
            })) catch oom();
            while (self.match_token(.TOKEN_COMMA)) {
                self.advance_assert(.TOKEN_COMMA);
                ident = self.parse_ident_expression();
                self.advance_assert(.TOKEN_COLON);
                ty = self.parse_type();
                params.append(self.create_node(AstNode{
                    .FUNCTION_PARAMETER = .{
                        .ident = ident,
                        .ty = ty,
                    }
                })) catch oom();
            }
            self.advance_assert(.TOKEN_RIGHT_PAREN);
        }

        const return_type = self.parse_type();
        self.symbol_type_table.put(name.IDENT_EXPRESSION.name, return_type) catch oom();

        const block = self.parse_block_statement();

        return self.create_node(AstNode{
            .FUNCTION_DEFINITION_STATEMENT = .{
                .ident =  name,
                .parameters = params,
                .block = block,
                .return_type = return_type,
            }
        });
    }

    fn parse_block_statement(self: *Self) *AstNode {
        var statements = AstNodeList.init(self.arena.allocator());
        self.advance_assert(.TOKEN_LEFT_BRACE);
        while (!self.match_token(.TOKEN_RIGHT_BRACE) and !self.match_token(.TOKEN_EOF)) {
            const stmt = self.parse_statement();
            if (stmt != undefined) statements.append(stmt) catch oom();
        }
        self.advance_assert(.TOKEN_RIGHT_BRACE);
        return self.create_node(AstNode{ .BLOCK_STATEMENT = .{
            .statements = statements,
        }});
    }

    fn create_node(self: *Self, a: AstNode) *AstNode {
        const node = self.arena.allocator().create(AstNode) catch unreachable;
        node.* = a;
        return node;
    }
};
