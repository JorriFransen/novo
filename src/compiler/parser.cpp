#include "parser.h"

#include <containers/darray.h>
#include <defines.h>
#include <memory/temp_allocator.h>
#include <nstring.h>

#include "ast.h"
#include "atom.h"
#include "instance.h"
#include "keywords.h"
#include "lexer.h"
#include "platform.h"
#include "scope.h"
#include "source_pos.h"

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

    Parser parser;
    parser.instance = instance;
    parser.lexer = &lexer;

    auto decls = temp_array_create<AST_Declaration *>(&instance->temp_allocator, 4);

    Scope *file_scope = scope_new(instance, Scope_Kind::FILE);

    while (!is_token(&lexer, TOK_EOF) && !lexer.error) {

        auto decl = parse_declaration(&parser, file_scope, true);
        if (!decl) return nullptr;
        darray_append(&decls, decl);
    }

    auto result = ast_file(instance, temp_array_finalize(&instance->ast_allocator, &decls), file_scope);

    temp_allocator_reset(&instance->temp_allocator_data, mark);

    return result;
}

AST_Declaration *parse_declaration(Parser *parser, Scope *scope, bool eat_semi)
{
    AST_Identifier *ident = parse_identifier(parser);

    // Report redeclarations in higher level scopes first
    if (scope_find_symbol(scope, ident->atom)) {
        auto name = atom_string(ident->atom);
        auto start = source_range_start(parser->instance, ident->range_id);

        auto ex_decl = scope_find_symbol(scope, ident->atom);
        assert(ex_decl);
        assert(ex_decl->ident);
        auto ex_start = source_range_start(parser->instance, ex_decl->ident->range_id);

        instance_fatal_error(parser->instance, start, "Redeclaration of symbol: '%s'", name.data);
        instance_fatal_error_note(parser->instance, ex_start, "Previous declaration was here");
        return nullptr;
    }

    return parse_declaration(parser, ident, scope, eat_semi);
}

AST_Declaration *parse_declaration(Parser *parser, AST_Identifier *ident, Scope *scope, bool eat_semi)
{
    AST_Declaration *result = nullptr;
    u64 end = 0;

    expect_token(parser, ':');


    AST_Type_Spec *ts = nullptr;
    if (!is_token(parser, ':') && !is_token(parser, '=')) {
        ts = parse_type_spec(parser);
        end = source_range_end(parser->instance, ts->range_id);
    }

    if (match_token(parser, ':')) {

        if (ts) {
            // TODO: report error
            assert(false);
        }
        result = parse_function_declaration(parser, ident, scope);

    } else {

        AST_Expression *init_expr = nullptr;
        if (match_token(parser, '=')) {
            init_expr = parse_expression(parser);
            end = source_range_end(parser->instance, init_expr->range_id);
        }

        if (eat_semi) {
            end = parser->lexer->token.source_pos_id;
            expect_token(parser, ';');
        }

        auto start = source_range_start(parser->instance, ident->range_id);
        auto range_id = source_range(parser->instance, start, end);
        result = ast_variable_declaration(parser->instance, ident, ts, init_expr, range_id);
    }

    if (!result) return nullptr;

    if (!scope_add_symbol(scope, ident->atom, result)) {
        auto name = atom_string(ident->atom);
        auto start = source_range_start(parser->instance, ident->range_id);

        auto ex_decl = scope_find_symbol(scope, ident->atom);
        assert(ex_decl);
        assert(ex_decl->ident);
        auto ex_start = source_range_start(parser->instance, ex_decl->ident->range_id);

        instance_fatal_error(parser->instance, start, "Redeclaration of symbol: '%s'", name.data);
        instance_fatal_error_note(parser->instance, ex_start, "Previous declaration was here");
        return nullptr;
    }
    return result;
}

AST_Declaration *parse_function_declaration(Parser *parser, AST_Identifier *ident, Scope *scope)
{
    assert(scope->kind == Scope_Kind::FILE);

    auto params = temp_array_create<AST_Declaration *>(&parser->instance->temp_allocator, 2);

    Scope *fn_scope = scope_new(parser->instance, Scope_Kind::FUNCTION_LOCAL, scope);

    expect_token(parser, '(');
    while (!is_token(parser, ')')) {
        if (params.array.count) {
            expect_token(parser, ',');
        }

        AST_Declaration *param_decl = parse_declaration(parser, fn_scope, false);
        if (!param_decl) return nullptr;

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

        auto stmt = parse_statement(parser, fn_scope);
        if (!stmt) return nullptr;

        darray_append(&stmts, stmt);
    }

    auto end = parser->lexer->token.source_pos_id;
    expect_token(parser, '}');

    auto body_array = temp_array_finalize(&parser->instance->ast_allocator, &stmts);

    auto start = source_range_start(parser->instance, ident->range_id);
    auto range = source_range(parser->instance, start, end);

    return ast_function_declaration(parser->instance, ident, params_array, body_array, return_ts, fn_scope, range);
}

AST_Expression *parse_leaf_expression(Parser *parser)
{
    auto ct = parser->lexer->token;
    auto range = source_range(parser->instance, ct.source_pos_id);

    if (is_token(parser, TOK_INT)) {
        next_token(parser->lexer);
        return ast_integer_literal_expression(parser->instance, ct.integer, range);

    } else if (is_token(parser, TOK_REAL)) {
        assert(false);

    } else if (is_token(parser, TOK_CHAR)) {
        next_token(parser->lexer);
        return ast_char_literal_expression(parser->instance, ct.character, range);

    } else if (is_token(parser, TOK_NAME)) {
        AST_Identifier *ident = parse_identifier(parser);
        return ast_identifier_expression(parser->instance, ident, range);

    } else if (is_token(parser, TOK_STRING)) {
        next_token(parser->lexer);
        return ast_string_literal_expression(parser->instance, ct.atom, range);

    } else if (is_token(parser, '(')) {
        next_token(parser->lexer);
        auto result = parse_expression(parser);
        expect_token(parser, ')');

        return result;
    }

    assert(false);
    return nullptr;
}

static bool is_binary_op(Token &token)
{
    switch ((char)token.kind) {
        case '+':
        case '-':
        case '*':
        case '/':
            return true;
    }

    return false;
}

static u64 get_precedence(Token &token)
{
    switch ((char)token.kind) {

        case '+':
        case '-':
            return 1;

        case '*':
        case '/':
            return 2;
    }

    assert(false);
    return 0;
}

static AST_Expression *parse_increasing_precedence(Parser *parser, AST_Expression *left, u64 min_prec)
{
    auto op_token = parser->lexer->token;

    if (!is_binary_op(op_token)) return left;

    auto new_prec = get_precedence(op_token);

    if (new_prec <= min_prec) {
        return left;
    } else {
        next_token(parser->lexer);
        auto right = parse_expression(parser, new_prec);

        auto start = source_range_start(parser->instance, left->range_id);
        auto end = source_range_end(parser->instance, right->range_id);
        auto range = source_range(parser->instance, start, end);

        return ast_binary_expression(parser->instance, op_token.kind, left, right, range);
    }
}

AST_Expression *parse_expression(Parser *parser, u64 min_prec/*=0*/)
{
    auto left = parse_leaf_expression(parser);

    while (true) {
        auto new_left = parse_increasing_precedence(parser, left, min_prec);
        if (left == new_left) return left;

        left = new_left;
    }
}

AST_Statement *parse_statement(Parser *parser, Scope *scope)
{
    if (is_token(parser, TOK_KEYWORD)) {
        return parse_keyword_statement(parser, scope);
    }

    // Remaining options start with identifier
    auto ident = parse_identifier(parser);
    if (is_token(parser, ':')) {
        auto decl = parse_declaration(parser, ident, scope, true);
        if (!decl) return nullptr;
        return ast_declaration_statement(parser->instance, decl, decl->range_id);
    } else {
        assert(false);
    }

    return nullptr;
    assert(false);
}

AST_Statement *parse_keyword_statement(Parser *parser, Scope *scope)
{
    auto ct = parser->lexer->token;
    assert(ct.kind == TOK_KEYWORD);
    next_token(parser->lexer);


    if (ct.atom == g_atom_return) {


        AST_Expression *expr = nullptr;
        if (!is_token(parser, ';')) {
            expr = parse_expression(parser);
        }
        auto end = parser->lexer->token.source_pos_id;
        expect_token(parser, ';');

        auto range = source_range(parser->instance, ct.source_pos_id, end);
        return ast_return_statement(parser->instance, expr, range);
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

    auto range = source_range(parser->instance, ident_tok.source_pos_id);
    return ast_identifier(parser->instance, ident_tok.atom, range);
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
        fprintf(stderr, "%s:%u:%u:", pos.name, pos.line, pos.offset);

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
