#include "parser.h"

#include "ast.h"
#include "keywords.h"
#include "lexer.h"
#include "platform.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>

namespace Novo {

AST_File *parse_file(Instance *instance, const String_Ref file_path)
{

    Lexer lexer;
    lexer_create(instance, &lexer);

    String file_content;

    auto mark = temp_allocator_get_mark(&instance->temp_allocator_data);

    bool read_ok = fs_read_entire_file(&instance->temp_allocator, file_path, &file_content);
    assert(read_ok);

    lexer_init_stream(&lexer, file_content, file_path);

    // while (!is_token(&lexer, TOK_EOF) && !lexer.error) {
    //     auto pos = instance->source_positions[lexer.token.source_pos_id];
    //     printf("%u:%u:%u: ", pos.line, pos.start, pos.length);
    //     printf("'%s' :\t'%s'\n", tmp_token_kind_str(lexer.token.kind).data, tmp_token_str(lexer.token).data);
    //     next_token(&lexer);
    // }
    //
    // temp_allocator_reset(&instance->temp_allocator_data, mark);
    //
    // return nullptr;

    Parser parser;
    parser.instance = instance;
    parser.lexer = &lexer;

    auto decls = temp_array_create<AST_Declaration *>(&instance->temp_allocator, 4);

    while (!is_token(&lexer, TOK_EOF) && !lexer.error) {

        auto decl = parse_declaration(&parser, true);
        darray_append(&decls, decl);
    }

    auto result = ast_file(instance, temp_array_finalize(&instance->ast_allocator, &decls));

    temp_allocator_reset(&instance->temp_allocator_data, mark);

    return result;
}

AST_Declaration *parse_declaration(Parser *parser, bool eat_semi)
{
    AST_Identifier *ident = parse_identifier(parser);
    return parse_declaration(parser, ident, eat_semi);
}

AST_Declaration *parse_declaration(Parser *parser, AST_Identifier *ident, bool eat_semi)
{
    expect_token(parser, ':');

    AST_Type_Spec *ts = nullptr;
    if (!is_token(parser, ':') && !is_token(parser, '=')) {
        ts = parse_type_spec(parser);
    }

    if (match_token(parser, ':')) {

        if (ts) {
            // TODO: report error
            assert(false);
        }
        return parse_function_declaration(parser, ident);

    } else {

        AST_Expression *init_expr = nullptr;
        if (match_token(parser, '=')) {
            init_expr = parse_expression(parser);
        }

        if (eat_semi) expect_token(parser, ';');

        return ast_variable_declaration(parser->instance, ident, ts, init_expr);
    }
}

AST_Declaration *parse_function_declaration(Parser *parser, AST_Identifier *ident)
{
    auto params = temp_array_create<AST_Declaration *>(&parser->instance->temp_allocator, 2);

    expect_token(parser, '(');
    while (!is_token(parser, ')')) {
        if (params.array.count) {
            expect_token(parser, ',');
        }

        AST_Declaration *param_decl = parse_declaration(parser, false);

        // TODO: report error
        assert(param_decl->kind == AST_Declaration_Kind::VARIABLE);

        darray_append(&params, param_decl);
    }
    expect_token(parser, ')');

    auto params_array = temp_array_finalize(&parser->instance->ast_allocator, &params);

    AST_Type_Spec *return_ts = nullptr;
    if (match_token(parser, TOK_RIGHT_ARROW)) {
        return_ts = parse_type_spec(parser);
    }

    auto stmts = temp_array_create<AST_Statement *>(&parser->instance->temp_allocator, 4);

    expect_token(parser, '{');
    while (!is_token(parser, '}')) {

        auto stmt = parse_statement(parser);
        if (!stmt) return nullptr;

        darray_append(&stmts, stmt);
    }
    expect_token(parser, '}');

    auto body_array = temp_array_finalize(&parser->instance->ast_allocator, &stmts);

    return ast_function_declaration(parser->instance, ident, params_array, body_array, return_ts);
}

AST_Expression *parse_expr_operand(Parser *parser)
{
    auto ct = parser->lexer->token;

    if (is_token(parser, TOK_INT)) {
        next_token(parser->lexer);
        return ast_integer_literal_expression(parser->instance, ct.integer);

    } else if (is_token(parser, TOK_REAL)) {
        assert(false);

    } else if (is_token(parser, TOK_CHAR)) {
        next_token(parser->lexer);
        return ast_char_literal_expression(parser->instance, ct.character);

    } else if (is_token(parser, TOK_NAME)) {
        AST_Identifier *ident = parse_identifier(parser);
        return ast_identifier_expression(parser->instance, ident);

    } else if (is_token(parser, TOK_STRING)) {
        next_token(parser->lexer);
        return ast_string_literal_expression(parser->instance, ct.atom);

    }

    assert(false);
    return nullptr;
}

AST_Expression *parse_expr_add(Parser *parser)
{
    AST_Expression *lhs = parse_expr_operand(parser);

    while (is_token(parser, '+') || is_token(parser, '-')) {

        char op = parser->lexer->token.kind;
        next_token(parser->lexer);

        AST_Expression *rhs = parse_expr_operand(parser);

        assert(op == '+' || op == '-');
        lhs = ast_binary_expression(parser->instance, op, lhs, rhs);

        // char op = cur_tok(parser).kind;
        // next_token(parser);
        //
        // AST_Expression *rhs = parse_expr_mul(parser);
        // return_if_null(rhs);
        //
        // auto ast_op = token_kind_to_ast_binop[(int)op];
        // debug_assert(ast_op != AST_Binary_Operator::INVALID);
        // lhs = ast_binary_expr_new(parser->context, {lhs->sr.start, rhs->sr.end}, ast_op, lhs, rhs);
    }

    return lhs;
}

AST_Expression *parse_expression(Parser *parser)
{
    return parse_expr_add(parser);
}

AST_Statement *parse_statement(Parser *parser)
{
    if (is_token(parser, TOK_KEYWORD)) {
        return parse_keyword_statement(parser);
    }

    // Remaining options start with identifier
    auto ident = parse_identifier(parser);
    if (is_token(parser, ':')) {
        auto decl = parse_declaration(parser, ident, true);
        return ast_declaration_statement(parser->instance, decl);
    } else {
        assert(false);
    }

    return nullptr;
    assert(false);
}

AST_Statement *parse_keyword_statement(Parser *parser)
{
    auto ct = parser->lexer->token;
    assert(ct.kind == TOK_KEYWORD);
    next_token(parser->lexer);

    if (ct.atom == g_atom_return) {
        AST_Expression *expr = nullptr;
        if (!is_token(parser, ';')) {
            expr = parse_expression(parser);
        }
        expect_token(parser, ';');
        return ast_return_statement(parser->instance, expr);
    }

    assert(false);
    return nullptr;
}

AST_Identifier *parse_identifier(Parser *parser)
{
    auto ident_tok = parser->lexer->token;

    if (!expect_token(parser, TOK_NAME)) {
        return nullptr;
    }

    return ast_identifier(parser->instance, ident_tok.atom);
}

AST_Type_Spec *parse_type_spec(Parser *parser)
{
    if (is_token(parser, TOK_NAME)) {
        AST_Identifier *ident = parse_identifier(parser);
        return ast_identifier_type_spec(parser->instance, ident);
    }

    assert(false);
    return nullptr;
}

bool expect_token(Parser *parser, Token_Kind kind)
{
    auto ct = parser->lexer->token;
    if (ct.kind != kind) {
        auto pos = parser->instance->source_positions[ct.source_pos_id];
        fprintf(stderr, "%s:%u:%u:", pos.name, pos.line, pos.start);

        auto tok_str = atom_string(ct.atom);
        fprintf(stderr, " error: Expected token '%s', got '%s'\n", tmp_token_kind_str(kind).data, tok_str.data);
        exit(1);
        return false;
    }

    next_token(parser->lexer);

    return true;
}

bool expect_token(Parser *parser, char c) {
    return expect_token(parser, (Token_Kind)c);
}

bool match_token(Parser *parser, Token_Kind kind)
{
    if (parser->lexer->token.kind == kind) {
        next_token(parser->lexer);
        return true;
    }

    return false;
}

bool match_token(Parser *parser, char c)
{
    return match_token(parser, (Token_Kind)c);
}

bool is_token(Parser *parser, Token_Kind kind)
{
    return is_token(parser->lexer, kind);
}

bool is_token(Parser *parser, char c)
{
    return is_token(parser->lexer, c);
}

}
