#include "ast_print.h"

#include <string_builder.h>

#include "ast.h"

namespace Novo {

static void ast_print_indent(String_Builder *sb, int indent);
static void ast_decl_to_string(String_Builder *sb, AST_Declaration *decl, int indent = 0);
static void ast_stmt_to_string(String_Builder *sb, AST_Statement *stmt, int indent = 0);
static void ast_expr_to_string(String_Builder *sb, AST_Expression *expr, int indent = 0);
static void ast_ts_to_string(String_Builder *sb, AST_Type_Spec *ts, int indent = 0);

String ast_to_string(AST_File *file, String_Builder *sb)
{
    for (s64 i = 0; i < file->declarations.count; i++) {
        ast_decl_to_string(sb, file->declarations[i]);
    }

    return string_builder_to_string(sb);
}

String ast_to_string(AST_File *file, Allocator *allocator)
{
    String_Builder sb;
    string_builder_init(&sb, allocator);

    String result = ast_to_string(file, &sb);

    string_builder_free(&sb);

    return result;
}

static void ast_print_indent(String_Builder *sb, int indent)
{
    for (int i = 0; i < indent; i++) {
        string_builder_append(sb, " ");
    }
}

static void ast_decl_to_string(String_Builder *sb, AST_Declaration *decl, int indent/*=0*/)
{
    assert(decl->ident);
    auto name = atom_string(decl->ident->atom);

    ast_print_indent(sb, indent);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {
            string_builder_append(sb, "VAR_DECL: '%s'\n", name.data);

            if (decl->variable.ts) {
                ast_ts_to_string(sb, decl->variable.ts, indent + 1);
            }

            if (decl->variable.init_expr) {
                ast_expr_to_string(sb, decl->variable.init_expr, indent + 1);
            }
            break;
        }

        case AST_Declaration_Kind::FUNCTION: {
            string_builder_append(sb, "FUNC_DECL: '%s'\n", name.data);

            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "PARAMS: %d\n", decl->function.params.count);

            for (s64 i = 0; i < decl->function.params.count; i++) {
                ast_decl_to_string(sb, decl->function.params[i], indent + 2);
            }

            if (decl->function.body.count) {
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "BODY:\n");
            }

            for (s64 i = 0; i < decl->function.body.count; i++) {
                ast_stmt_to_string(sb, decl->function.body[i], indent + 2);
            }

            string_builder_append(sb, "\n");
            break;
        }

    }
}

static void ast_stmt_to_string(String_Builder *sb, AST_Statement *stmt, int indent/*=0*/)
{
    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            ast_decl_to_string(sb, stmt->declaration, indent);
            break;
        }

        case AST_Statement_Kind::RETURN: {
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_RETURN: \n");

            if (stmt->return_expr) {
                ast_expr_to_string(sb, stmt->return_expr, indent + 1);
            }
            break;
        }
    }
}

static void ast_expr_to_string(String_Builder *sb, AST_Expression *expr, int indent/*=0*/)
{
    ast_print_indent(sb, indent);

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            auto str = atom_string(expr->identifier->atom);
            string_builder_append(sb, "EXPR_IDENT: '%s'\n", str.data);
            break;
        }

        case AST_Expression_Kind::BINARY: {
            string_builder_append(sb, "EXPR_BINARY: '%c'\n", expr->binary.op);
            ast_expr_to_string(sb, expr->binary.lhs, indent + 1);
            ast_expr_to_string(sb, expr->binary.rhs, indent + 1);
            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL: {
            string_builder_append(sb, "EXPR_INT: %llu\n", expr->integer_literal);
            break;
        }

        case AST_Expression_Kind::CHAR_LITERAL: {
            string_builder_append(sb, "EXPR_CHAR: '%c'\n", expr->char_literal);
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: {
            auto str = atom_string(expr->string_literal);
            string_builder_append(sb, "EXPR_STR: %s\n", str.data);
            break;
        }
    }
}

static void ast_ts_to_string(String_Builder *sb, AST_Type_Spec *ts, int indent/*=0*/)
{
    ast_print_indent(sb, indent);

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false); break;

        case AST_Type_Spec_Kind::IDENTIFIER: {
            auto str = atom_string(ts->identifier->atom);
            string_builder_append(sb, "IDENT_TS: '%s'\n", str.data);
            break;
        }
    }
}

}
