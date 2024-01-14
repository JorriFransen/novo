#include "ast_print.h"

#include <containers/darray.h>
#include <defines.h>
#include <nstring.h>
#include <string_builder.h>

#include "ast.h"
#include "atom.h"
#include "instance.h"
#include "source_pos.h"

#include <assert.h>

namespace Novo {

static void ast_print_pos(Instance *instance, String_Builder *sb, u32 pos_id_a, u32 pos_id_b);
static void ast_print_pos(Instance *instance, String_Builder *sb, u32 range_id);

static void ast_print_indent(String_Builder *sb, int indent);
static void ast_decl_to_string(Instance *instance, String_Builder *sb, AST_Declaration *decl, int indent = 0);
static void ast_stmt_to_string(Instance *instance, String_Builder *sb, AST_Statement *stmt, int indent = 0);
static void ast_expr_to_string(Instance *instance, String_Builder *sb, AST_Expression *expr, int indent = 0);
static void ast_ts_to_string(Instance *instance, String_Builder *sb, AST_Type_Spec *ts, int indent = 0);

String ast_to_string(Instance *instance, AST_File *file, String_Builder *sb)
{
    for (s64 i = 0; i < file->declarations.count; i++) {
        ast_decl_to_string(instance, sb, file->declarations[i]);
    }

    return string_builder_to_string(sb);
}

String ast_to_string(Instance *instance, AST_File *file, Allocator *allocator)
{
    String_Builder sb;
    string_builder_init(&sb, allocator);

    String result = ast_to_string(instance, file, &sb);

    string_builder_free(&sb);

    return result;
}

static void ast_print_pos(Instance *instance, String_Builder *sb, u32 pos_id_a, u32 pos_id_b)
{
    assert(pos_id_a > 0 && pos_id_a < instance->source_positions.count);
    assert(pos_id_b > 0 && pos_id_b < instance->source_positions.count);

    auto start = &instance->source_positions[pos_id_a];

    if (pos_id_a == pos_id_b) {
        string_builder_append(sb, "%03d:%03d-%07d  ", start->line, start->offset, start->length);
    } else {
        auto end = &instance->source_positions[pos_id_b];

        string_builder_append(sb, "%03d:%03d-%03d:%03d  ", start->line, start->offset, end->line, end->offset);
    }
}

static void ast_print_pos(Instance *instance, String_Builder *sb, u32 range_id)
{
    if (range_id) {

        assert(range_id > 0 && range_id < instance->source_ranges.count);
        auto range = &instance->source_ranges[range_id];
        ast_print_pos(instance, sb, range->start_index, range->end_index);

    } else {
        string_builder_append(sb, "                 ");
    }
}

static void ast_print_indent(String_Builder *sb, int indent)
{
    for (int i = 0; i < indent; i++) {
        string_builder_append(sb, " ");
    }
}

static void ast_decl_to_string(Instance *instance, String_Builder *sb, AST_Declaration *decl, int indent/*=0*/)
{
    ast_print_pos(instance, sb, decl->range_id);

    assert(decl->ident);
    auto name = atom_string(decl->ident->atom);

    ast_print_indent(sb, indent);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {
            string_builder_append(sb, "VAR_DECL: '%s'\n", name.data);

            if (decl->variable.ts) {
                ast_ts_to_string(instance, sb, decl->variable.ts, indent + 1);
            }

            if (decl->variable.init_expr) {
                ast_expr_to_string(instance, sb, decl->variable.init_expr, indent + 1);
            }
            break;
        }

        case AST_Declaration_Kind::FUNCTION: {
            string_builder_append(sb, "FUNC_DECL: '%s'\n", name.data);

            if (decl->function.return_ts) {
                ast_print_pos(instance, sb, 0);
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "RETURN_TS:\n");
                ast_ts_to_string(instance, sb, decl->function.return_ts, indent + 2);
            }

            ast_print_pos(instance, sb, decl->function.param_range_id);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "PARAMS: %d\n", decl->function.params.count);

            for (s64 i = 0; i < decl->function.params.count; i++) {
                ast_decl_to_string(instance, sb, decl->function.params[i], indent + 2);
            }

            ast_print_pos(instance, sb, decl->function.body_range_id);
            if (decl->function.body.count) {
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "BODY:\n");
            }

            for (s64 i = 0; i < decl->function.body.count; i++) {
                ast_stmt_to_string(instance, sb, decl->function.body[i], indent + 2);
            }

            string_builder_append(sb, "\n");
            break;
        }

    }
}

static void ast_stmt_to_string(Instance *instance, String_Builder *sb, AST_Statement *stmt, int indent/*=0*/)
{

    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            ast_decl_to_string(instance, sb, stmt->declaration, indent);
            break;
        }

        case AST_Statement_Kind::CALL: {
            ast_expr_to_string(instance, sb, stmt->call, indent);
            break;
        }

        case AST_Statement_Kind::RETURN: {
            ast_print_pos(instance, sb, stmt->range_id);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_RETURN:\n");

            if (stmt->return_expr) {
                ast_expr_to_string(instance, sb, stmt->return_expr, indent + 1);
            }
            break;
        }
    }
}

static void ast_expr_to_string(Instance *instance, String_Builder *sb, AST_Expression *expr, int indent/*=0*/)
{
    ast_print_pos(instance, sb, expr->range_id);

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
            ast_expr_to_string(instance, sb, expr->binary.lhs, indent + 1);
            ast_expr_to_string(instance, sb, expr->binary.rhs, indent + 1);
            break;
        }

        case AST_Expression_Kind::CALL: {
            string_builder_append(sb, "EXPR_CALL:\n");
            ast_print_pos(instance, sb, 0);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "BASE:\n");
            ast_expr_to_string(instance, sb, expr->call.base, indent + 2);

            ast_print_pos(instance, sb, 0);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "ARGS: %d\n", expr->call.args.count);

            for (s64 i = 0; i < expr->call.args.count; i++) {
                ast_expr_to_string(instance, sb, expr->call.args[i], indent + 2);
            }
            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL: {
            string_builder_append(sb, "EXPR_INT: %llu\n", expr->integer_literal);
            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: {
            string_builder_append(sb, "EXPR_REAL: %f/%f\n", expr->real_literal.r32, expr->real_literal.r64);
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

static void ast_ts_to_string(Instance *instance, String_Builder *sb, AST_Type_Spec *ts, int indent/*=0*/)
{
    ast_print_pos(instance, sb, ts->range_id);
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
