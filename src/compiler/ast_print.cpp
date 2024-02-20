#include "ast_print.h"

#include <containers/darray.h>
#include <containers/hash_table.h>
#include <defines.h>
#include <nstring.h>
#include <string_builder.h>

#include "ast.h"
#include "atom.h"
#include "instance.h"
#include "source_pos.h"
#include "token.h"

#include <assert.h>

namespace Novo {

static void ast_print_pos(Instance* instance, String_Builder* sb, Source_Pos pos);
static void ast_print_pos(Instance* instance, String_Builder* sb, AST_Declaration* decl);
static void ast_print_pos(Instance* instance, String_Builder* sb, AST_Expression* expr);
static void ast_print_pos(Instance* instance, String_Builder* sb, AST_Statement* stmt);
static void ast_print_pos(Instance* instance, String_Builder* sb, AST_Identifier* ident);
static void ast_print_pos(Instance* instance, String_Builder* sb, AST_Type_Spec* ts);

static void ast_print_indent(String_Builder* sb, int indent);
static void ast_decl_to_string(Instance* instance, String_Builder* sb, AST_Declaration* decl, int indent = 0);
static void ast_stmt_to_string(Instance* instance, String_Builder* sb, AST_Statement* stmt, int indent = 0);
static void ast_expr_to_string(Instance* instance, String_Builder* sb, AST_Expression* expr, int indent = 0);
static void ast_ts_to_string(Instance* inst, String_Builder* sb, AST_Type_Spec* ts, int indent = 0);

String ast_to_string(Instance* instance, AST_File* file, String_Builder* sb)
{
    for (s64 i = 0; i < file->nodes.count; i++) {
        auto &node = file->nodes[i];

        switch (node.kind) {

            case AST_Node_Kind::INVALID: assert(false); break;

            case AST_Node_Kind::DECLARATION: {
                ast_decl_to_string(instance, sb, node.declaration);
                break;
            }

            case AST_Node_Kind::STATEMENT: {
                assert(node.statement->kind == AST_Statement_Kind::IMPORT);
                ast_stmt_to_string(instance, sb, node.statement);
                break;
            }

            case AST_Node_Kind::EXPRESSION: assert(false); break;
            case AST_Node_Kind::TYPE_SPEC: assert(false); break;
        }

        string_builder_append(sb, "\n");
    }

    return string_builder_to_string(sb);
}

String ast_to_string(Instance* instance, AST_File* file, Allocator* allocator)
{
    String_Builder sb;
    string_builder_init(&sb, allocator);

    String result = ast_to_string(instance, file, &sb);

    string_builder_free(&sb);

    return result;
}

static void ast_print_pos(Instance* instance, String_Builder* sb, Source_Pos pos)
{

    Imported_File file = instance->imported_files[pos.file_index];

    Source_Pos end = { pos.file_index, pos.offset + pos.length - 1, 2 };

    Line_Info start_li = line_info(file.newline_offsets, pos.offset);
    Line_Info end_li = line_info(file.newline_offsets, end.offset);

    String file_name = atom_string(file.name);

    if (start_li.line == end_li.line) {
        string_builder_append(sb, "%s:%03u:%03u-%07u: ", file_name.data, start_li.line, start_li.offset, pos.length);
    } else {
        string_builder_append(sb, "%s:%03u:%03u-%03u:%03u: ", file_name.data, start_li.line, start_li.offset, end_li.line, end_li.offset);
    }

}

static void ast_print_pos(Instance* instance, String_Builder* sb, AST_Declaration* decl)
{
    Source_Pos pos = source_pos(instance, decl);
    ast_print_pos(instance, sb, pos);
}

static void ast_print_pos(Instance* instance, String_Builder* sb, AST_Expression* expr)
{
    Source_Pos pos = source_pos(instance, expr);
    ast_print_pos(instance, sb, pos);
}

static void ast_print_pos(Instance* instance, String_Builder* sb, AST_Statement* stmt)
{
    Source_Pos pos = source_pos(instance, stmt);
    ast_print_pos(instance, sb, pos);
}

static void ast_print_pos(Instance* instance, String_Builder* sb, AST_Identifier* ident)
{
    Source_Pos pos = source_pos(instance, ident);
    ast_print_pos(instance, sb, pos);
}

static void ast_print_pos(Instance* instance, String_Builder* sb, AST_Type_Spec* ts)
{
    Source_Pos pos = source_pos(instance, ts);
    ast_print_pos(instance, sb, pos);
}

static void ast_print_indent(String_Builder* sb, int indent)
{
    for (int i = 0; i < indent; i++) {
        string_builder_append(sb, " ");
    }
}

static void ast_decl_to_string(Instance* instance, String_Builder* sb, AST_Declaration* decl, int indent/*=0*/)
{
    ast_print_pos(instance, sb, decl);

    assert(decl->ident);
    auto name = atom_string(decl->ident->atom);

    ast_print_indent(sb, indent);

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;
        case AST_Declaration_Kind::BUILTIN_TYPE: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {
            string_builder_append(sb, "VAR_DECL: '%s'\n", name.data);

            if (decl->variable.type_spec) {
                ast_print_pos(instance, sb, decl->variable.type_spec);
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "TS:\n");
                ast_ts_to_string(instance, sb, decl->variable.type_spec, indent + 2);
            }

            if (decl->variable.init_expr) {
                ast_print_pos(instance, sb, decl->variable.init_expr);
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "INIT:\n");
                ast_expr_to_string(instance, sb, decl->variable.init_expr, indent + 2);
            }
            break;
        }

        case AST_Declaration_Kind::STRUCT_MEMBER: {
            string_builder_append(sb, "STRUCT_MEMBER: '%s'\n", name.data);
            assert(decl->variable.type_spec);
            ast_ts_to_string(instance, sb, decl->variable.type_spec, indent + 1);
            if (decl->variable.init_expr) {
                ast_expr_to_string(instance, sb, decl->variable.init_expr, indent + 1);
            }
            break;
        };

        case AST_Declaration_Kind::STRUCT: {
            string_builder_append(sb, "STRUCT_DECL: '%s'\n", name.data);
            for (s64 i = 0; i < decl->structure.fields.count; i++) {
                ast_decl_to_string(instance, sb, decl->structure.fields[i], indent + 1);
            }
            break;
        };

        case AST_Declaration_Kind::FUNCTION: {

            if (decl->flags & AST_DECL_FLAG_FOREIGN) {
                string_builder_append(sb, "FOREIGN_");
            }

            string_builder_append(sb, "FUNC_DECL: '%s'\n", name.data);

            if (decl->function.params.count) {

                Source_Pos param_pos = source_pos(instance, decl->function.params[0]);
                if (decl->function.params.count > 1) {
                    param_pos = source_pos(param_pos, source_pos(instance, decl->function.params[decl->function.params.count - 1]));
                }

                ast_print_pos(instance, sb, param_pos);
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "PARAMS: %d\n", decl->function.params.count);

                for (s64 i = 0; i < decl->function.params.count; i++) {
                    ast_decl_to_string(instance, sb, decl->function.params[i], indent + 2);
                }
            }

            if (decl->function.return_ts) {
                ast_print_pos(instance, sb, decl->function.return_ts);
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "RETURN_TS:\n");
                ast_ts_to_string(instance, sb, decl->function.return_ts, indent + 2);
            }

            if (decl->function.body.count) {
                Source_Pos body_pos;
                bool found = hash_table_find(&instance->function_body_positions, decl, &body_pos);
                assert(found);

                ast_print_pos(instance, sb, body_pos);
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "BODY:\n");
            }

            for (s64 i = 0; i < decl->function.body.count; i++) {
                ast_stmt_to_string(instance, sb, decl->function.body[i], indent + 2);
            }
            break;
        }
    }
}

static void ast_stmt_to_string(Instance* instance, String_Builder* sb, AST_Statement* stmt, int indent/*=0*/)
{

    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;

        case AST_Statement_Kind::IMPORT: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_IMPORT: '%s'\n", stmt->import_path.data);
            break;
        }

        case AST_Statement_Kind::DECLARATION: {
            ast_decl_to_string(instance, sb, stmt->declaration, indent);
            break;
        }

        case AST_Statement_Kind::ASSIGNMENT: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_ASSIGN:\n");

            ast_print_pos(instance, sb, stmt->assignment.lvalue);
            ast_print_indent(sb, indent);
            string_builder_append(sb, " LVALUE:\n");
            ast_expr_to_string(instance, sb, stmt->assignment.lvalue, indent + 2);

            ast_print_pos(instance, sb, stmt->assignment.rvalue);
            ast_print_indent(sb, indent);
            string_builder_append(sb, " RVALUE:\n");
            ast_expr_to_string(instance, sb, stmt->assignment.rvalue, indent + 2);
            break;
        }

        case AST_Statement_Kind::ARITHMETIC_ASSIGNMENT: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_ARITHMETIC_ASSIGN: '%s'\n", tmp_token_kind_str((Token_Kind)stmt->arithmetic_assignment.op).data);

            ast_print_pos(instance, sb, stmt->arithmetic_assignment.lvalue);
            ast_print_indent(sb, indent);
            string_builder_append(sb, " LVALUE:\n");
            ast_expr_to_string(instance, sb, stmt->arithmetic_assignment.lvalue, indent + 2);

            ast_print_pos(instance, sb, stmt->arithmetic_assignment.rvalue);
            ast_print_indent(sb, indent);
            string_builder_append(sb, " RVALUE:\n");
            ast_expr_to_string(instance, sb, stmt->arithmetic_assignment.rvalue, indent + 2);
            break;
        }

        case AST_Statement_Kind::CALL: {
            ast_expr_to_string(instance, sb, stmt->call, indent);
            break;
        }

        case AST_Statement_Kind::RETURN: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_RETURN:\n");

            if (stmt->return_expr) {
                ast_expr_to_string(instance, sb, stmt->return_expr, indent + 1);
            }
            break;
        }

        case AST_Statement_Kind::IF: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_IF:\n");

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                auto if_block = stmt->if_stmt.blocks[i];

                ast_print_pos(instance, sb, if_block.cond);
                ast_print_indent(sb, indent + 1);
                if (i == 0) {
                    string_builder_append(sb, "IF_COND:\n");
                } else {
                    string_builder_append(sb, "ELSE_IF_COND:\n");
                }

                ast_expr_to_string(instance, sb, if_block.cond, indent + 2);

                ast_print_pos(instance, sb, if_block.then);
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "THEN:\n");

                ast_stmt_to_string(instance, sb, if_block.then, indent + 2);
            }

            if (stmt->if_stmt.else_stmt) {
                ast_print_pos(instance, sb,stmt->if_stmt.else_stmt);
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "ELSE:\n");
                ast_stmt_to_string(instance, sb, stmt->if_stmt.else_stmt, indent + 2);
            }
            break;
        }

        case AST_Statement_Kind::WHILE: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_WHILE:\n");

            ast_print_pos(instance, sb, stmt->while_stmt.cond);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "COND:\n");

            ast_expr_to_string(instance, sb, stmt->while_stmt.cond, indent + 2);

            ast_print_pos(instance, sb, stmt->while_stmt.stmt);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "DO:\n");

            ast_stmt_to_string(instance, sb, stmt->while_stmt.stmt, indent + 2);
            break;
        }

        case AST_Statement_Kind::FOR: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_FOR:\n");

            ast_print_pos(instance, sb, stmt->for_stmt.init);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "INIT:\n");

            ast_stmt_to_string(instance, sb, stmt->for_stmt.init, indent + 2);

            ast_print_pos(instance, sb, stmt->for_stmt.cond);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "COND:\n");

            ast_expr_to_string(instance, sb, stmt->for_stmt.cond, indent + 2);

            ast_print_pos(instance, sb, stmt->for_stmt.step);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "STEP:\n");

            ast_stmt_to_string(instance, sb, stmt->for_stmt.step, indent + 2);

            ast_print_pos(instance, sb, stmt->for_stmt.step);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "DO:\n");

            ast_stmt_to_string(instance, sb, stmt->for_stmt.stmt, indent + 2);
            break;
        }

        case AST_Statement_Kind::BREAK: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_BREAK:\n");
            break;
        }

        case AST_Statement_Kind::CONTINUE: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_CONTINUE:\n");
            break;
        }

        case AST_Statement_Kind::RUN: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_RUN:\n");
            ast_expr_to_string(instance, sb, stmt->run.expression, indent + 1);
            break;
        }

        case AST_Statement_Kind::INSERT: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_INSERT:\n");
            ast_expr_to_string(instance, sb, stmt->insert.expression, indent + 1);
            break;
        }

        case AST_Statement_Kind::BLOCK: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_BLOCK:\n");

            for (s64 i = 0; i < stmt->block.statements.count; i++) {
                ast_stmt_to_string(instance, sb, stmt->block.statements[i], indent + 1);
            }
            break;
        }

        case AST_Statement_Kind::ASSERT: {
            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "STMT_ASSERT:\n");

            ast_print_pos(instance, sb, stmt);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "COND:\n");
            ast_expr_to_string(instance, sb, stmt->assert_stmt.cond, indent + 2);

            if (stmt->assert_stmt.message) {
                ast_print_pos(instance, sb, stmt);
                ast_print_indent(sb, indent + 1);
                string_builder_append(sb, "MESSAGE:\n");
                ast_expr_to_string(instance, sb, stmt->assert_stmt.message, indent + 2);
            }
            break;
        }
    }
}

static void ast_expr_to_string(Instance* inst, String_Builder* sb, AST_Expression* expr, int indent/*=0*/)
{
    ast_print_pos(inst, sb, expr);

    ast_print_indent(sb, indent);

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            auto str = atom_string(expr->identifier->atom);
            string_builder_append(sb, "EXPR_IDENT: '%s'\n", str.data);
            break;
        }

        case AST_Expression_Kind::UNARY: {
            string_builder_append(sb, "EXPR_UNARY: '%s'\n", tmp_token_kind_str((Token_Kind)expr->unary.op).data);
            ast_expr_to_string(inst, sb, expr->unary.operand, indent + 1);
            break;
        }

        case AST_Expression_Kind::BINARY: {
            string_builder_append(sb, "EXPR_BINARY: '%s'\n", tmp_token_kind_str((Token_Kind)expr->binary.op).data);
            ast_expr_to_string(inst, sb, expr->binary.lhs, indent + 1);
            ast_expr_to_string(inst, sb, expr->binary.rhs, indent + 1);
            break;
        }

        case AST_Expression_Kind::MEMBER: {
            string_builder_append(sb, "EXPR_MEMBER:\n");

            ast_print_pos(inst, sb, expr->member.base);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "BASE:\n");
            ast_expr_to_string(inst, sb, expr->member.base, indent + 2);

            ast_print_pos(inst, sb, expr->member.member_name);
            ast_print_indent(sb, indent);
            string_builder_append(sb, "MEMBER_NAME: '%s'\n", atom_string(expr->member.member_name->atom).data);
            break;
        }

        case AST_Expression_Kind::CALL: {
            string_builder_append(sb, "EXPR_CALL:\n");
            ast_print_pos(inst, sb, expr->call.base);
            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "BASE:\n");
            ast_expr_to_string(inst, sb, expr->call.base, indent + 2);

            if (expr->call.args.count) {
                ast_print_pos(inst, sb, expr->call.args[0]);
            } else {
                ast_print_pos(inst, sb, expr->call.base);
            }

            ast_print_indent(sb, indent + 1);
            string_builder_append(sb, "ARGS: %d\n", expr->call.args.count);

            for (s64 i = 0; i < expr->call.args.count; i++) {
                ast_expr_to_string(inst, sb, expr->call.args[i], indent + 2);
            }
            break;
        }

        case AST_Expression_Kind::ADDRESS_OF: {
            string_builder_append(sb, "EXPR_ADDRESS_OF:\n");
            ast_expr_to_string(inst, sb, expr->call.base, indent + 1);
            break;
        }

        case AST_Expression_Kind::DEREF: {
            string_builder_append(sb, "DEREF:\n");
            ast_expr_to_string(inst, sb, expr->unary.operand, indent + 1);
            break;
        }

        case AST_Expression_Kind::CAST: {
            string_builder_append(sb, "CAST:\n");
            ast_ts_to_string(inst, sb, expr->cast.ts, indent + 1);
            ast_expr_to_string(inst, sb, expr->cast.operand, indent + 1);
            break;
        }

        case AST_Expression_Kind::COMPOUND: {
            string_builder_append(sb, "EXPR_COMPOUND: (%d members)\n", expr->compound.expressions.count);

            for (s64 i = 0; i < expr->compound.expressions.count; i++) {
                ast_expr_to_string(inst, sb, expr->compound.expressions[i], indent + 1);
            }
            break;
        }

        case AST_Expression_Kind::RUN: {
            string_builder_append(sb, "EXPR_RUN:\n");
            ast_expr_to_string(inst, sb, expr->run.expression, indent + 1);
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

        case AST_Expression_Kind::BOOL_LITERAL: {
            string_builder_append(sb, "EXPR_BOOL: '%s'\n", expr->bool_literal ? "true" : "false");
            break;
        }

        case AST_Expression_Kind::NULL_LITERAL: {
            string_builder_append(sb, "EXPR_NULL\n");
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: {
            auto str = atom_string(expr->string_literal);
            string_builder_append(sb, "EXPR_STR: \"%s\"\n", str.data);
            break;
        }
    }
}

static void ast_ts_to_string(Instance* inst, String_Builder* sb, AST_Type_Spec* ts, int indent/*=0*/)
{
    ast_print_pos(inst, sb, ts);
    ast_print_indent(sb, indent);

    switch (ts->kind) {
        case AST_Type_Spec_Kind::INVALID: assert(false); break;

        case AST_Type_Spec_Kind::IDENTIFIER: {
            auto str = atom_string(ts->identifier->atom);
            string_builder_append(sb, "IDENT_TS: '%s'\n", str.data);
            break;
        }

        case AST_Type_Spec_Kind::POINTER: {
            string_builder_append(sb, "POINTER_TS:\n");
            ast_ts_to_string(inst, sb, ts->base, indent + 1);
            break;
        }
    }
}

}
