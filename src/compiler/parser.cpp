#include "parser.h"

#include <containers/darray.h>
#include <containers/hash_table.h>
#include <memory/temp_allocator.h>
#include <platform.h>

#include "ast.h"
#include "atom.h"
#include "filesystem.h"
#include "instance.h"
#include "keywords.h"
#include "scope.h"
#include "source_pos.h"
#include "token.h"

#include <cassert>

namespace Novo {

#define expect_token(p, k) if (!expect_token_internal((p), (k))) return {};

Parser parser_create(Instance* inst, const String_Ref file_path, s64 import_index)
{
    String file_content;

    auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);

    bool read_ok = fs_read_entire_file(&inst->temp_allocator, file_path, &file_content);
    assert(read_ok);

    Parser parser;
    parser.instance = inst;

    lexer_create(inst, &parser.lexer);
    lexer_init_stream(&parser.lexer, file_content, file_path, import_index, 0);

    parser.file_read_mark = mark;
    parser.cwd = fs_dirname(&inst->temp_allocator, file_path);
    if (!string_ends_with(parser.cwd, NPLATFORM_PATH_SEPARATOR)) {
        parser.cwd = string_append(&inst->temp_allocator, parser.cwd, NPLATFORM_PATH_SEPARATOR);
    }

    parser.next_index_in_function = 0;
    parser.parsing_function_body = false;
    parser.new_expr_flags = AST_EXPR_FLAG_NONE;

    return parser;
}

Parser parser_create(Instance* inst, const String_Ref name, const String_Ref content, s64 import_index, u32 offset)
{
    Parser parser;
    parser.instance = inst;

    lexer_create(inst, &parser.lexer);
    lexer_init_stream(&parser.lexer, content, name, import_index, offset);

    parser.file_read_mark = 0;

    String_Ref file_path = atom_string(inst->imported_files[import_index].name);
    parser.cwd = fs_dirname(&inst->temp_allocator, file_path);
    if (!string_ends_with(parser.cwd, NPLATFORM_PATH_SEPARATOR)) {
        parser.cwd = string_append(&inst->temp_allocator, parser.cwd, NPLATFORM_PATH_SEPARATOR);
    }

    parser.next_index_in_function = 0;
    parser.parsing_function_body = false;
    parser.new_expr_flags = AST_EXPR_FLAG_NONE;

    return parser;
}

AST_File* parse_file(Instance* inst, const String_Ref file_path, s64 import_index)
{
    Parser parser = parser_create(inst, file_path, import_index);

    DArray<AST_Node> nodes = parse_nodes(inst, &parser, inst->global_scope, Parse_Context::FILE_SCOPE);

    AST_File* result = ast_file(inst, nodes);

    temp_allocator_reset(&inst->temp_allocator_data, parser.file_read_mark);

    return result;
}

DArray<AST_Node> parse_string(Instance* inst, const String_Ref name, const String_Ref content, Scope* scope, Parse_Context context, s64 import_index, u32 offset)
{
    Parser parser = parser_create(inst, name, content, import_index, offset);

    auto result = parse_nodes(inst, &parser, scope, context);

    return result;
}

DArray<AST_Node> parse_nodes(Instance* inst, Parser* parser, Scope* scope, Parse_Context context)
{
    if (context == Parse_Context::FILE_SCOPE) {
        return parse_file_nodes(inst, parser, scope);
    } else {
        assert(context == Parse_Context::LOCAL_STATEMENT_SCOPE);
        return parse_statement_nodes(inst, parser, scope);
    }
}

DArray<AST_Node> parse_file_nodes(Instance* inst, Parser* parser, Scope* scope)
{
    Lexer* lexer = &parser->lexer;

    auto nodes = temp_array_create<AST_Node>(&inst->temp_allocator, 4);

    while (!is_token(lexer, TOK_EOF) && !is_token(lexer, TOK_ERROR)) {

        Token ct = parser->lexer.token;
        switch (ct.kind) {

            case '#': {
                next_token(&parser->lexer);

                Token name_token = parser->lexer.token;
                assert(name_token.kind == Token_Kind::TOK_NAME);
                next_token(&parser->lexer);

                switch (name_token.keyword) {
                    default: {
                        assert(false && "Unhandled directive");
                    }

                    case Novo_Keyword::KW_import: {
                        auto str_tok = parser->lexer.token;
                        expect_token(parser, TOK_STRING);

                        String str_lit = atom_string(str_tok.atom);

                        String import_path = string_append(&inst->ast_allocator, parser->cwd, String_Ref(str_lit.data + 1, str_lit.length - 2));
                        assert(fs_is_file(import_path));

                        AST_Statement* stmt = ast_import_statement(inst, import_path);
                        if (!stmt) return {};
                        darray_append(&nodes, ast_node(stmt));

                        break;
                    }

                    case Novo_Keyword::KW_run: {
                        AST_Expression_Flags old_flags = parser->new_expr_flags;
                        parser->new_expr_flags |= AST_EXPR_FLAG_CHILD_OF_RUN;

                        AST_Expression* expr = parse_expression(parser);
                        expect_token(parser, ';');

                        parser->new_expr_flags = old_flags;

                        if (expr->kind != AST_Expression_Kind::CALL) {
                            instance_fatal_error(parser->instance, source_pos(parser->instance, expr), "Expected call expression after #run");
                        }

                        AST_Statement* run_stmt = ast_run_statement(parser->instance, expr);

                        Source_Pos pos = source_pos(source_pos(parser, ct), source_pos(parser->instance, expr));
                        save_source_pos(parser->instance, run_stmt, pos);

                        darray_append(&nodes, ast_node(run_stmt));

                        break;
                    }

                    case Novo_Keyword::KW_insert: {

                        AST_Expression_Flags old_flags = parser->new_expr_flags;
                        parser->new_expr_flags |= AST_EXPR_FLAG_CHILD_OF_RUN;

                        AST_Expression* expr = parse_expression(parser);
                        expect_token(parser, ';');

                        parser->new_expr_flags = old_flags;

                        if (expr->kind != AST_Expression_Kind::CALL) {
                            instance_fatal_error(parser->instance, source_pos(parser->instance, expr), "Expected call expression after #insert");
                        }

                        AST_Statement* insert_stmt = ast_insert_statement(parser->instance, expr);

                        Source_Pos pos = source_pos(source_pos(parser, ct), source_pos(parser->instance, expr));
                        save_source_pos(parser->instance, insert_stmt, pos);

                        darray_append(&nodes, ast_node(insert_stmt));

                        break;
                    }
                }

                break;
            }

            default: {

                AST_Declaration* decl = parse_declaration(parser, scope, true);
                if (!decl) return {};
                darray_append(&nodes, ast_node(decl));

                break;
            }
        }
    }

    DArray<AST_Node> result = temp_array_finalize(&inst->ast_allocator, &nodes);

    return result;
}

DArray<AST_Node> parse_statement_nodes(Instance* inst, Parser* parser, Scope* scope)
{
    Lexer* lexer = &parser->lexer;

    auto nodes = temp_array_create<AST_Node>(&inst->temp_allocator, 4);

    while (!is_token(lexer, TOK_EOF) && !is_token(lexer, TOK_ERROR)) {

        AST_Statement* stmt = parse_statement(parser, scope, true);
        darray_append(&nodes, ast_node(stmt));
    }

    DArray<AST_Node> result = temp_array_finalize(&inst->ast_allocator, &nodes);

    return result;
}

AST_Declaration* parse_declaration(Parser* parser, Scope* scope, bool eat_semi)
{
    AST_Identifier* ident = parse_identifier(parser);

    return parse_declaration(parser, ident, scope, eat_semi);
}

AST_Declaration* parse_declaration(Parser* parser, AST_Identifier* ident, Scope* scope, bool eat_semi)
{
    // Report redeclarations in higher level scopes first
    if (scope_find_symbol(scope, ident->atom, nullptr)) {
        auto name = atom_string(ident->atom);

        auto ex_decl = scope_find_symbol(scope, ident->atom, nullptr);
        assert(ex_decl);
        assert(ex_decl->ident);

        Source_Pos ident_pos = source_pos(parser->instance, ident);
        Source_Pos decl_pos = source_pos(parser->instance, ex_decl);

        instance_error(parser->instance, ident_pos, "Redeclaration of symbol: '%s'", name.data);
        instance_fatal_error_note(parser->instance, decl_pos, "Previous declaration was here");
        return nullptr;
    }

    if (parser->parsing_function_body && parser->current_function_name == ident->atom) {
        auto name = atom_string(ident->atom);

        Source_Pos ident_pos = source_pos(parser->instance, ident);

        instance_fatal_error(parser->instance, ident_pos, "Declaration shadows parent function: '%s'", name.data);
        return nullptr;
    }

    Source_Pos pos = source_pos(parser->instance, ident);

    AST_Declaration* result = nullptr;

    expect_token(parser, ':');

    AST_Type_Spec* ts = nullptr;
    if (!is_token(parser, ':') && !is_token(parser, '=')) {
        ts = parse_type_spec(parser);
        pos = source_pos(pos, source_pos(parser->instance, ts));
    }

    if (match_token(parser, ':')) {

        if (ts) {
            // TODO: report error
            assert(false);
        }

        if (match_keyword(parser, g_keyword_struct)) {
            result = parse_struct_declaration(parser, ident, scope);
        } else {
            result = parse_function_declaration(parser, ident, scope);
        }

    } else {

        AST_Expression* init_expr = nullptr;
        if (match_token(parser, '=')) {
            init_expr = parse_expression(parser);
            if (!init_expr) return nullptr;
            pos = source_pos(pos, source_pos(parser->instance, init_expr));
        }

        if (eat_semi) {
            expect_token(parser, ';');
        }

        result = ast_variable_declaration(parser->instance, ident, ts, init_expr);
        if (parser->parsing_function_body) {
            result->variable.index = parser->next_index_in_function++;
        }
        save_source_pos(parser->instance, result, pos);
    }

    if (!result) return nullptr;

    bool scope_add_res = scope_add_symbol(scope, ident->atom, result, SCOPE_FIND_OPTS_NO_REDECL_CHECK);
    assert(scope_add_res);

    return result;
}

AST_Declaration* parse_struct_declaration(Parser* parser, AST_Identifier* ident, Scope* scope)
{
    Source_Pos pos = source_pos(parser->instance, ident);

    expect_token(parser, '{');

    auto fields = temp_array_create<AST_Declaration*>(&parser->instance->temp_allocator, 8);
    Scope* struct_scope = scope_new(parser->instance, Scope_Kind::STRUCT, scope);

    while (!is_token(parser, '}')) {

        Source_Pos field_pos = source_pos(&parser->lexer);

        AST_Identifier* name = parse_identifier(parser);
        expect_token(parser, ':');
        AST_Type_Spec* ts = parse_type_spec(parser);

        field_pos = source_pos(field_pos, source_pos(parser->instance, ts));

        AST_Expression* default_value = nullptr;
        if (match_token(parser, '=')) {
            default_value = parse_expression(parser);
            field_pos = source_pos(field_pos, source_pos(parser->instance, default_value));
        }

        expect_token(parser, ';');

        auto mem_decl = ast_struct_member_declaration(parser->instance, name, ts, default_value);

        if (!scope_add_symbol(struct_scope, name->atom, mem_decl, SCOPE_FIND_OPTS_LIMIT_TO_STRUCT)) {
            auto new_name = atom_string(name->atom);

            auto ex_decl = scope_find_symbol(struct_scope, name->atom, nullptr);
            assert(ex_decl);
            assert(ex_decl->ident);

            Source_Pos ident_pos = source_pos(parser->instance, ident);
            Source_Pos decl_pos = source_pos(parser->instance, ex_decl);

            instance_error(parser->instance, ident_pos, "Redeclaration of symbol: '%s'", new_name.data);
            instance_fatal_error_note(parser->instance, decl_pos, "Previous declaration was here");
            return nullptr;
        }

        save_source_pos(parser->instance, mem_decl, field_pos);

        darray_append(&fields, mem_decl);
    }

    pos = source_pos(pos, source_pos(&parser->lexer));
    expect_token(parser, '}');

    auto fields_array = temp_array_finalize(&parser->instance->ast_allocator, &fields);


    AST_Declaration* result = ast_struct_declaration(parser->instance, ident, fields_array, struct_scope);
    save_source_pos(parser->instance, result, pos);
    return result;
}

AST_Declaration* parse_function_declaration(Parser* parser, AST_Identifier* ident, Scope* scope)
{
    assert(scope->kind == Scope_Kind::GLOBAL);

    Source_Pos pos = source_pos(parser->instance, ident);

    auto params = temp_array_create<AST_Declaration*>(&parser->instance->temp_allocator, 2);
    Scope* fn_scope = scope_new(parser->instance, Scope_Kind::FUNCTION_LOCAL, scope);

    u32 arg_index = 0;
    bool c_vararg = false;

    expect_token(parser, '(');
    while (!is_token(parser, ')')) {
        if (params.array.count) {
            expect_token(parser, ',');
        }

        if (match_token(parser, TOK_DOT_DOT)) {
            c_vararg = true;
            break;
        }

        AST_Declaration* param_decl = parse_declaration(parser, fn_scope, false);
        if (!param_decl) return nullptr;

        // TODO: report error
        assert(param_decl->kind == AST_Declaration_Kind::VARIABLE);
        param_decl->flags |= AST_DECL_FLAG_PARAM;
        param_decl->variable.index = arg_index++;

        darray_append(&params, param_decl);
    }

    AST_Declaration_Flags flags = AST_DECL_FLAG_NONE;

    if (c_vararg) {
        if (!match_token(parser, ')')) {
            instance_fatal_error(parser->instance, source_pos(&parser->lexer), "Expected ')' after '..' while parsing foreign vararg function");
        }

        flags |= AST_DECL_FLAG_FOREIGN_VARARG;
    } else {
        expect_token(parser, ')');
    }

    auto params_array = temp_array_finalize(&parser->instance->ast_allocator, &params);

    AST_Type_Spec* return_ts = nullptr;

    Token ct = parser->lexer.token;
    if (ct.kind != '#' && ct.kind != '{' && ct.kind != ';') {
        expect_token(parser, TOK_RIGHT_ARROW);
        return_ts = parse_type_spec(parser);
    }

    DArray<AST_Statement*> body_array = {};

    Source_Pos body_pos = source_pos(&parser->lexer);
    if (is_token(parser, '{')) {

        if (c_vararg) {
            Source_Pos pos = source_pos(&parser->lexer);
            instance_fatal_error(parser->instance, pos, "Expected '#foreign' while parsing foreign vararg function, got '}'");
        }

        auto stmts = temp_array_create<AST_Statement*>(&parser->instance->temp_allocator, 4);

        assert(parser->next_index_in_function == 0); // TODO: push/pop when dealing with nested functions
        assert(!parser->parsing_function_body);
        parser->parsing_function_body = true;
        parser->current_function_name = ident->atom;

        expect_token(parser, '{');
        while (!is_token(parser, '}')) {

            auto stmt = parse_statement(parser, fn_scope, true);
            if (!stmt) return nullptr;

            darray_append(&stmts, stmt);
        }

        Source_Pos current_pos = source_pos(&parser->lexer);
        pos = source_pos(pos, current_pos);
        body_pos = source_pos(body_pos, current_pos);
        expect_token(parser, '}');

        parser->next_index_in_function = 0;
        assert(parser->parsing_function_body);
        parser->parsing_function_body = false;
        parser->current_function_name = 0;

        body_array = temp_array_finalize(&parser->instance->ast_allocator, &stmts);
    } else {

        expect_token(parser, '#');

        auto directive_name = parser->lexer.token;
        expect_token(parser, TOK_NAME);
        assert(directive_name.atom == atom_get("foreign"));

        pos = source_pos(pos, source_pos(&parser->lexer));
        expect_token(parser, ';');

        flags |= AST_DECL_FLAG_FOREIGN;
    }

    if (c_vararg && ! (flags & AST_DECL_FLAG_FOREIGN)) {
        instance_fatal_error(parser->instance, source_pos(&parser->lexer), "Expected directive '#foreign' while parsing foreign varargs function");
    }

    AST_Declaration* result = ast_function_declaration(parser->instance, ident, params_array, body_array, return_ts, fn_scope);
    result->flags |= flags;
    save_source_pos(parser->instance, result, pos);
    if (body_array.count) hash_table_add(&parser->instance->function_body_positions, result, body_pos);
    return result;
}

AST_Expression* parse_leaf_expression(Parser* parser)
{
    Source_Pos pos = source_pos(&parser->lexer);
    auto ct = parser->lexer.token;

    if (ct.kind == TOK_ERROR) {
        return nullptr;
    }

    AST_Expression* result = nullptr;

    switch ((u32)ct.kind) {

        case '-': {
            next_token(&parser->lexer);
            AST_Expression* operand = parse_leaf_expression(parser);

            result = ast_unary_expression(parser->instance, '-', operand);
            break;
        }

        case TOK_INT: {
            next_token(&parser->lexer);

            bool hex = ct.flags & TOK_FLAG_HEX;
            bool binary = ct.flags & TOK_FLAG_BINARY;

            assert(!(binary && hex));

            result = ast_integer_literal_expression(parser->instance, ct.integer);

            if (hex) {
                result->flags |= AST_EXPR_FLAG_HEX_LITERAL;
            } else if (binary) {
                result->flags |= AST_EXPR_FLAG_BINARY_LITERAL;
            }
            break;
        }

        case TOK_REAL: {
            next_token(&parser->lexer);
            result = ast_real_literal_expression(parser->instance, ct.real);
            break;
        }

        case TOK_CHAR: {
            next_token(&parser->lexer);
            result = ast_char_literal_expression(parser->instance, ct.character);
            break;
        }

        case TOK_NAME: {
            result = ast_identifier_expression(parser->instance, parse_identifier(parser));
            break;
        }

        case TOK_STRING: {
            next_token(&parser->lexer);

            String_Ref stripped = atom_string(ct.atom);
            stripped.length -= 2;
            stripped.data += 1;
            Atom string_atom = atom_get(stripped);
            result = ast_string_literal_expression(parser->instance, string_atom);
            break;
        }

        case TOK_KEYWORD: {

            next_token(&parser->lexer);

            switch (ct.keyword) {

                default: assert(false && "Invalid keyword in expression"); break;

                case Novo_Keyword::KW_true: {
                    result = ast_bool_literal_expression(parser->instance, true);
                    break;
                }

                case Novo_Keyword::KW_false: {
                    result = ast_bool_literal_expression(parser->instance, false);
                    break;
                }

                case Novo_Keyword::KW_null: {
                    result = ast_null_literal_expression(parser->instance);
                    break;
                }

                case Novo_Keyword::KW_cast: {

                    expect_token(parser, '(');
                    AST_Type_Spec* ts = parse_type_spec(parser);
                    expect_token(parser, ',');
                    AST_Expression* operand = parse_expression(parser);

                    pos = source_pos(pos, source_pos(&parser->lexer));
                    expect_token(parser, ')');

                    result = ast_cast_expression(parser->instance, ts, operand);
                    break;
                }

                case Novo_Keyword::KW_sizeof: {

                    AST_Expression* expr = nullptr;

                    expect_token(parser, '(');

                    if (match_token(parser, ':')) {
                        AST_Type_Spec* ts = parse_type_spec(parser);
                        expr = ast_type_expression(parser->instance, ts);
                        save_source_pos(parser->instance, expr, source_pos(parser->instance, ts));

                    } else {
                        if (!is_token(parser, TOK_NAME)) {
                            instance_fatal_error(parser->instance, source_pos(&parser->lexer), "Expected identifier or ':' to specify type in 'sizeof()'");
                        }
                        expr = parse_expression(parser);
                    }
                    expect_token(parser, ')');

                    assert(expr);

                    pos = source_pos(pos, source_pos(&parser->lexer));

                    result = ast_sizeof_expression(parser->instance, expr);

                    break;
                }

                case Novo_Keyword::KW_alignof: {

                    AST_Expression* expr = nullptr;

                    expect_token(parser, '(');

                    if (match_token(parser, ':')) {
                        AST_Type_Spec* ts = parse_type_spec(parser);
                        expr = ast_type_expression(parser->instance, ts);
                        save_source_pos(parser->instance, expr, source_pos(parser->instance, ts));
                    } else {

                        if (!is_token(parser, TOK_NAME)) {
                            instance_fatal_error(parser->instance, source_pos(&parser->lexer), "Expected identifier or ':' to specify type in 'sizeof()'");
                        }
                        expr = parse_expression(parser);
                    }

                    expect_token(parser, ')');

                    result = ast_alignof_expression(parser->instance, expr);

                    break;
                }

                case Novo_Keyword::KW_offsetof: {

                    expect_token(parser, '(');
                    AST_Identifier* struct_ident = parse_identifier(parser);
                    expect_token(parser, ',');
                    AST_Identifier* member_ident = parse_identifier(parser);
                    expect_token(parser, ')');

                    assert(struct_ident);
                    assert(member_ident);

                    result = ast_offsetof_expression(parser->instance, struct_ident, member_ident);
                    pos = source_pos(pos, source_pos(&parser->lexer));

                    break;
                }

            }

            assert(result);
            break;
        }

        case '*': {
            next_token(&parser->lexer);

            AST_Expression *operand = parse_leaf_expression(parser);
            result = ast_address_of_expression(parser->instance, operand);

            pos = source_pos(pos, source_pos(parser->instance, operand));
            break;
        }

        case '<': {
            next_token(&parser->lexer);

            AST_Expression *operand = parse_leaf_expression(parser);
            result = ast_deref_expression(parser->instance, operand);

            pos = source_pos(pos, source_pos(parser->instance, operand));
            break;
        }

        case '{': {
            next_token(&parser->lexer);

            auto exprs = temp_array_create<AST_Expression*>(&parser->instance->temp_allocator, 4);

            while (!is_token(parser, '}')) {

                if (exprs.array.count) {
                    expect_token(parser, ',');
                }

                AST_Expression* expr = parse_expression(parser);
                darray_append(&exprs, expr);
            }

            pos = source_pos(pos, source_pos(&parser->lexer));
            expect_token(parser, '}');

            DArray<AST_Expression*> expr_array = temp_array_finalize(&parser->instance->ast_allocator, &exprs);

            result = ast_compound_expression(parser->instance, expr_array);

            break;
        }

        case '(': {
            next_token(&parser->lexer);
            result = parse_expression(parser);
            pos = source_pos(pos, source_pos(&parser->lexer));
            expect_token(parser, ')');
            break;
        }

        case '#': {
            next_token(&parser->lexer);
            Token name_tok = parser->lexer.token;
            assert(name_tok.kind == Token_Kind::TOK_NAME);
            next_token(&parser->lexer);

            switch (name_tok.keyword) {
                default: {
                    instance_fatal_error(parser->instance, source_pos(&parser->lexer), "Invalid directive at expression level. Expected 'run' after '#', got: '%s'", tmp_token_str(parser->lexer.token).data);
                    break;
                }

                case Novo_Keyword::KW_run: {
                    AST_Expression_Flags old_flags = parser->new_expr_flags;
                    parser->new_expr_flags |= AST_EXPR_FLAG_CHILD_OF_RUN;

                    AST_Expression* expr = parse_expression(parser);

                    parser->new_expr_flags = old_flags;


                    if (expr->kind != AST_Expression_Kind::CALL) {
                        instance_fatal_error(parser->instance, source_pos(parser->instance, expr), "Expected call expression after #run");
                    }


                    result = ast_run_expression(parser->instance, expr);
                    pos = source_pos(pos, source_pos(&parser->lexer));

                    break;
                }
            }

            break;
        }

        default: {
            assert(!result);
            auto tok_str = atom_string(ct.atom);
            instance_fatal_error(parser->instance, source_pos(parser, ct), "Unexpected token '%s' when parsing leaf expression",  tok_str.data);
            return nullptr;
        }
    }

    assert(result);

    result->flags |= parser->new_expr_flags;

    save_source_pos(parser->instance, result, pos);

    while (is_token(parser, '(') || is_token(parser, '.')) {

        switch ((u32)parser->lexer.token.kind) {
            case '(': {
                next_token(&parser->lexer);

                auto args_temp = temp_array_create<AST_Expression*>(&parser->instance->temp_allocator);


                while (!is_token(parser, ')')) {
                    if (args_temp.array.count) {
                        expect_token(parser, ',');
                    }

                    AST_Expression* arg_expr = parse_expression(parser);
                    darray_append(&args_temp, arg_expr);
                }

                pos = source_pos(pos, source_pos(&parser->lexer));
                next_token(&parser->lexer);

                auto args = temp_array_finalize(&parser->instance->ast_allocator, &args_temp);

                result = ast_call_expression(parser->instance, result, args);

                save_source_pos(parser->instance, result, pos);
                break;
            }

            case '.': {
                next_token(&parser->lexer);

                pos = source_pos(pos, source_pos(&parser->lexer));
                AST_Identifier* member_name = parse_identifier(parser);

                result = ast_member_expression(parser->instance, result, member_name);

                save_source_pos(parser->instance, result, pos);
                break;
            }

            default: assert(false); break;
        }

        assert(result);

    result->flags |= parser->new_expr_flags;
    }

    return result;
}

static AST_Expression* parse_increasing_precedence(Parser* parser, AST_Expression* left, u64 min_prec)
{
    Source_Pos pos = source_pos(parser->instance, left);

    auto op_token = parser->lexer.token;

    if (!is_binary_op(op_token.kind)) return left;

    auto new_prec = get_precedence(op_token.kind);

    if (new_prec <= min_prec) {
        return left;
    } else {
        next_token(&parser->lexer);
        auto right = parse_expression(parser, new_prec);

        AST_Expression* result = ast_binary_expression(parser->instance, op_token.kind, left, right);
        save_source_pos(parser->instance, result, source_pos(pos, source_pos(parser->instance, right)));
        return result;
    }
}

AST_Expression* parse_expression(Parser* parser, u64 min_prec/*=0*/)
{
    auto left = parse_leaf_expression(parser);

    while (true) {
        auto new_left = parse_increasing_precedence(parser, left, min_prec);
        if (left == new_left) return left;

        left = new_left;
    }
}

AST_Statement* parse_statement(Parser* parser, Scope* scope, bool eat_semi)
{
    if (parser->lexer.token.kind == TOK_ERROR) return nullptr;

    Source_Pos pos = source_pos(&parser->lexer);

    if (match_token(parser, '{')) {

        Scope* block_scope = scope_new(parser->instance, Scope_Kind::FUNCTION_LOCAL, scope);

        auto stmts = temp_array_create<AST_Statement*>(&parser->instance->temp_allocator, 8);
        while (!is_token(parser, '}')) {
            AST_Statement* stmt = parse_statement(parser, block_scope, true);
            darray_append(&stmts, stmt);
        }

        pos = source_pos(pos, source_pos(&parser->lexer));
        expect_token(parser, '}');

        auto stmt_array = temp_array_finalize(&parser->instance->ast_allocator, &stmts);
        AST_Statement* result = ast_block_statement(parser->instance, stmt_array, block_scope);
        save_source_pos(parser->instance, result, pos);
        return result;

    } else if (match_token(parser, '#')) {

        Token name_tok = parser->lexer.token;
        assert(name_tok.kind == Token_Kind::TOK_NAME);
        next_token(&parser->lexer);

        switch (name_tok.keyword) {
            default: {
                instance_fatal_error(parser->instance, source_pos(&parser->lexer), "Invalid directive at statement level: '%s'", tmp_token_str(parser->lexer.token).data);
                break;
            }

            case Novo_Keyword::KW_insert: {
                AST_Expression_Flags old_flags = parser->new_expr_flags;
                parser->new_expr_flags |= AST_EXPR_FLAG_CHILD_OF_RUN;

                AST_Expression* expr = parse_expression(parser);
                expect_token(parser, ';');

                parser->new_expr_flags = old_flags;

                if (expr->kind != AST_Expression_Kind::CALL) {
                    instance_fatal_error(parser->instance, source_pos(parser->instance, expr), "Expected call expression after #insert");
                }

                AST_Statement* insert_stmt = ast_insert_statement(parser->instance, expr);

                pos = source_pos(pos, source_pos(parser->instance, expr));
                save_source_pos(parser->instance, insert_stmt, pos);

                return insert_stmt;
            }
        }

    } else if (is_token(parser, TOK_KEYWORD)) {
        return parse_keyword_statement(parser, scope);
    }

    AST_Statement* result = nullptr;

    // Remaining options start with (unary?) expression
    AST_Expression* expr = parse_leaf_expression(parser);
    if (!expr) return nullptr;

    if (expr->kind == AST_Expression_Kind::CALL) {
        result = ast_call_expr_statement(parser->instance, expr);
        pos = source_pos(parser->instance, expr);
        if (eat_semi) expect_token(parser, ';');

    } else if (expr->kind == AST_Expression_Kind::IDENTIFIER && is_token(parser, ':')) {
        auto decl = parse_declaration(parser, expr->identifier, scope,true);
        pos = source_pos(parser->instance, decl);
        if (!decl) return nullptr;
        result = ast_declaration_statement(parser->instance, decl);

    } else if (match_token(parser, '=')) {
        auto value = parse_expression(parser);

        pos = source_pos(pos, source_pos(parser->instance, value));

        if (eat_semi) expect_token(parser, ';');

        result = ast_assignment_statement(parser->instance, expr, value);

    } else if (is_binary_arithmetic_op(parser->lexer.token.kind)) {

        u32 op = parser->lexer.token.kind;
        next_token(&parser->lexer);
        expect_token(parser, '=');

        AST_Expression* rhs = parse_expression(parser);
        pos = source_pos(pos, source_pos(parser->instance, rhs));

        if (eat_semi) expect_token(parser, ';');

        result = ast_arithmetic_assignment_statement(parser->instance, op, expr, rhs);

    } else {

        if (expr->kind == AST_Expression_Kind::RUN) {
            instance_fatal_error(parser->instance, source_pos(parser->instance, expr), "Invalid #run, use as expression, or as statement on file level (global scope)");
        }
        assert(false);
    }

    assert(result);

    save_source_pos(parser->instance, result, pos);

    return result;
}

AST_Statement* parse_keyword_statement(Parser* parser, Scope* scope)
{
    Source_Pos pos = source_pos(&parser->lexer);
    auto ct = parser->lexer.token;
    AST_Statement* result = nullptr;

    assert(ct.kind == TOK_KEYWORD);
    next_token(&parser->lexer);

    switch (ct.keyword) {

        default: {
            instance_fatal_error(parser->instance, source_pos(parser, ct), "Unexpected keyword '%s'", atom_string(ct.atom).data);
            assert(false);
            break;
        }

        case Novo_Keyword::KW_if: {

            AST_Expression* cond = parse_expression(parser);
            AST_Statement* then_stmt = parse_statement(parser, scope, true);

            auto if_blocks = temp_array_create<AST_If_Block>(&parser->instance->temp_allocator);

            darray_append(&if_blocks, { cond, then_stmt });

            AST_Statement* else_stmt = nullptr;

            while (match_keyword(parser, g_keyword_else)) {

                if (match_keyword(parser, g_keyword_if)) {

                    AST_Expression* elif_cond = parse_expression(parser);
                    AST_Statement* elif_stmt = parse_statement(parser, scope, true);

                    darray_append(&if_blocks, { elif_cond, elif_stmt });
                } else {

                    else_stmt = parse_statement(parser, scope, true);

                    break;
                }
            }

            if (else_stmt) {
                pos = source_pos(pos, source_pos(parser->instance, else_stmt));
            } else {
                pos = source_pos(pos, source_pos(parser->instance, if_blocks[if_blocks.array.count - 1].then));
            }

            auto if_blocks_array = temp_array_finalize(&parser->instance->ast_allocator, &if_blocks);

            result = ast_if_statement(parser->instance, if_blocks_array, else_stmt);
            break;
        }

        break;

        case Novo_Keyword::KW_while: {

            bool expect_close_paren = match_token(parser, '(');

            AST_Expression* cond = parse_expression(parser);

            if (expect_close_paren) {
                expect_token(parser, ')');
            }

            AST_Statement* stmt = parse_statement(parser, scope, true);

            result = ast_while_statement(parser->instance, cond, stmt);

            pos = source_pos(pos, source_pos(parser->instance, stmt));
            break;
        }

        case Novo_Keyword::KW_for: {

            bool expect_close_paren = match_token(parser, '(');

            Scope* for_scope = scope_new(parser->instance, Scope_Kind::FUNCTION_LOCAL, scope);

            AST_Statement* init_stmt = parse_statement(parser, for_scope, true);
            AST_Expression* cond = parse_expression(parser);
            expect_token(parser, ';');
            AST_Statement* step_stmt = parse_statement(parser, for_scope, false);

            if (expect_close_paren) {
                expect_token(parser, ')');
            } else {
                expect_token(parser, ';');
            }

            AST_Statement* do_stmt = parse_statement(parser, for_scope, true);

            assert(init_stmt);
            assert(cond);
            assert(step_stmt);
            assert(do_stmt);

            result = ast_for_statement(parser->instance, init_stmt, cond, step_stmt, do_stmt, for_scope);

            pos = source_pos(pos, source_pos(parser->instance, do_stmt));
            break;
        }

        case Novo_Keyword::KW_break: {

            expect_token(parser, ';');

            result = ast_break_statement(parser->instance);
            break;
        }

        case Novo_Keyword::KW_continue: {
            expect_token(parser, ';');

            result = ast_continue_statement(parser->instance);
            break;
        }

        case Novo_Keyword::KW_return: {
            AST_Expression* expr = nullptr;
            if (!is_token(parser, ';')) {
                expr = parse_expression(parser);
                pos = source_pos(pos, source_pos(parser->instance, expr));
            }
            expect_token(parser, ';');

            result = ast_return_statement(parser->instance, expr);
            break;
        }

        case Novo_Keyword::KW_assert: {

            expect_token(parser, '(');
            AST_Expression* cond = parse_expression(parser);

            AST_Expression* message = nullptr;
            if (match_token(parser, ',')) {
                message = parse_expression(parser);
            }

            expect_token(parser, ')');
            pos = source_pos(pos, source_pos(&parser->lexer));
            expect_token(parser, ';');

            result = ast_assert_statement(parser->instance, cond, message);
            break;
        }
    }

    assert(result);
    save_source_pos(parser->instance, result, pos);
    return result;
}

AST_Identifier* parse_identifier(Parser* parser)
{
    auto ident_tok = parser->lexer.token;

    expect_token(parser, TOK_NAME);

    AST_Identifier* result = ast_identifier(parser->instance, ident_tok.atom);

    if (parser->parsing_function_body) {
        result->index = parser->next_index_in_function++;
    }

    save_source_pos(parser->instance, result, source_pos(parser, ident_tok));

    return result;
}

AST_Type_Spec* parse_type_spec(Parser* parser)
{
    auto ct = parser->lexer.token;

    AST_Type_Spec *result = nullptr;

    Source_Pos pos = source_pos(&parser->lexer);

    switch (ct.kind) {
        case TOK_NAME: {
            AST_Identifier* ident = parse_identifier(parser);
            result = ast_identifier_type_spec(parser->instance, ident);
            break;
        }

        case '*': {
            next_token(&parser->lexer);
            AST_Type_Spec* base = parse_type_spec(parser);
            result = ast_pointer_type_spec(parser->instance, base);

            pos = source_pos(pos, source_pos(parser->instance, base));
            break;
        }

        default: {

            instance_fatal_error(parser->instance, source_pos(parser, ct), "Unexpected token when parsing type: '%s'", tmp_token_str(ct));

        }
    }

    save_source_pos(parser->instance, result, pos);

    assert(result);
    return result;
}

bool expect_token_internal(Parser* parser, Token_Kind kind)
{
    auto ct = parser->lexer.token;
    if (ct.kind != kind) {
        auto tok_str = atom_string(ct.atom);
        instance_fatal_error(parser->instance, source_pos(parser, ct), "Expected token '%s', got '%s'", tmp_token_kind_str(kind).data, tok_str.data);
        return false;
    }

    next_token(&parser->lexer);

    return true;
}

bool expect_token_internal(Parser* parser, char c) {
    return expect_token_internal(parser, (Token_Kind)c);
}

bool match_token(Parser* parser, Token_Kind kind)
{
    if (parser->lexer.token.kind == kind) {
        next_token(&parser->lexer);
        return true;
    }

    return false;
}

bool match_token(Parser* parser, char c)
{
    return match_token(parser, (Token_Kind)c);
}

bool match_name(Parser* parser, const char* name)
{
    if (parser->lexer.token.kind == TOK_NAME && parser->lexer.token.atom == atom_get(name)) {
        next_token(&parser->lexer);
        return true;
    }

    return false;
}

bool match_name(Parser* parser, Atom name)
{
    if (parser->lexer.token.kind == TOK_NAME && parser->lexer.token.atom == name) {
        next_token(&parser->lexer);
        return true;
    }

    return false;
}

bool match_keyword(Parser* parser, Atom kw_atom)
{
    if (parser->lexer.token.kind == TOK_KEYWORD && parser->lexer.token.atom == kw_atom) {
        next_token(&parser->lexer);
        return true;
    }

    return false;
}

bool is_token(Parser* parser, Token_Kind kind)
{
    return is_token(&parser->lexer, kind);
}

bool is_token(Parser* parser, char c)
{
    return is_token(&parser->lexer, c);
}

bool is_keyword(Parser* parser, Atom kw_atom)
{
    return parser->lexer.token.kind == TOK_KEYWORD && parser->lexer.token.atom == kw_atom;
}

u64 get_precedence(Token_Kind op)
{
    switch ((u32)op) {

        case '<':
        case '>':
        case TOK_EQ:
        case TOK_NEQ:
        case TOK_LTEQ:
        case TOK_GTEQ:
            return 1;

        case '+':
        case '-':
            return 2;

        case '*':
        case '/':
            return 3;

    }

    assert(false);
    return 0;
}


}
