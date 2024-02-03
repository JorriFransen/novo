#include "typer.h"

#include <containers/darray.h>
#include <memory/temp_allocator.h>
#include <nstring.h>

#include "ast.h"
#include "instance.h"
#include "lexer.h"
#include "scope.h"
#include "source_pos.h"
#include "task.h"
#include "type.h"

#include <assert.h>

namespace Novo {

bool type_node(Instance *inst, Type_Task *task, AST_Node *node, Scope *scope)
{
    switch (node->kind) {

        case AST_Node_Kind::INVALID: assert(false); break;

        case AST_Node_Kind::DECLARATION: {
            return type_declaration(inst, task, node->declaration, scope);
        }

        case AST_Node_Kind::STATEMENT: {
            return type_statement(inst, task, node->statement, scope);
        }

        case AST_Node_Kind::EXPRESSION: assert(false); break;

        case AST_Node_Kind::TYPE_SPEC: {
            return type_type_spec(inst, task, node->ts, scope);
        }
    }

    assert(false);
    return false;
}

bool type_declaration(Instance *inst, Type_Task *task, AST_Declaration *decl, Scope *scope)
{
    if (decl->flags & AST_DECL_FLAG_TYPED) {
        assert(decl->resolved_type);
        return true;
    }

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            Type *result_type = nullptr;

            if (decl->variable.type_spec) {
                if (!type_type_spec(inst, task, decl->variable.type_spec, scope)) {
                    return false;
                }

                result_type = decl->variable.type_spec->resolved_type;
            }

            if (decl->variable.init_expr) {
                if (!type_expression(inst, task, decl->variable.init_expr, scope, result_type)) {
                    return false;
                }

                if (!result_type) {
                    result_type = decl->variable.init_expr->resolved_type;
                }
            }

            assert(result_type);

            decl->resolved_type = result_type;
            break;
        }

        case AST_Declaration_Kind::STRUCT_MEMBER: {
            if (!type_type_spec(inst, task, decl->variable.type_spec, scope)) {
                return false;
            }

            assert(!decl->variable.init_expr);
            decl->resolved_type = decl->variable.type_spec->resolved_type;
            break;
        }

        case AST_Declaration_Kind::STRUCT: {

            Scope *struct_scope = decl->structure.scope;
            auto &fields = decl->structure.fields;

            for (s64 i = 0; i < fields.count; i++) {
                auto field = fields[i];

                if (!type_declaration(inst, task, field, struct_scope)) {
                    return false;
                }
            }

            auto member_types = temp_array_create<Type *>(&inst->temp_allocator, fields.count);

            for (s64 i = 0; i < fields.count; i++) {
                auto field = fields[i];
                darray_append(&member_types, field->resolved_type);
            }

            decl->resolved_type = struct_type_new(inst, member_types, struct_scope);

            temp_array_destroy(&member_types);
            break;
        }

        case AST_Declaration_Kind::FUNCTION: {

            for (s64 i = 0; i < decl->function.params.count; i++) {
                if (!(decl->function.params[i]->flags & AST_DECL_FLAG_TYPED)) {
                    return false;
                }
            }

            if (decl->function.return_ts && !(decl->function.return_ts->flags & AST_TS_FLAG_TYPED)) {
                return false;
            }

            // Check the body, we might be able to type this function already, but might not be ready to move on to ssa...

            for (s64 i = 0; i < decl->function.body.count; i++) {
                if (!(decl->function.body[i]->flags & AST_STMT_FLAG_TYPED)) {
                    return false;
                }
            }

            auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);
            auto param_types = temp_array_create<Type *>(&inst->temp_allocator, decl->function.params.count);

            for (s64 i = 0; i < decl->function.params.count; i++) {
                darray_append(&param_types, decl->function.params[i]->resolved_type);
            }

            Type *return_type = decl->function.return_ts ? decl->function.return_ts->resolved_type : inst->builtin_type_void;

            decl->resolved_type = function_type_get(inst, param_types, return_type);

            temp_allocator_reset(&inst->temp_allocator_data, mark);
            break;
        }

        case AST_Declaration_Kind::BUILTIN_TYPE: assert(false); break;
    }

    assert(decl->resolved_type);
    decl->flags |= AST_DECL_FLAG_TYPED;
    return true;
}

bool type_statement(Instance *inst, Type_Task *task, AST_Statement *stmt, Scope *scope)
{
    if (stmt->flags & AST_TS_FLAG_TYPED) {
        return true;
    }

    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;

        case AST_Statement_Kind::IMPORT: break;

        case AST_Statement_Kind::DECLARATION: {
            if (!type_declaration(inst, task, stmt->declaration, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::ASSIGNMENT: {

            AST_Expression *lvalue = stmt->assignment.lvalue;
            AST_Expression *rvalue = stmt->assignment.rvalue;

            if (!type_expression(inst, task, lvalue, scope, nullptr)) {
                return false;
            }

            if (!type_expression(inst, task, rvalue, scope, lvalue->resolved_type)) {
                return false;
            }

            if (lvalue->resolved_type != rvalue->resolved_type) {
                auto sp_id = source_range_start(inst, stmt->range_id);
                instance_fatal_error(inst, sp_id, "Mismatching types in assignment, left: '%s', right: '%s'",
                        temp_type_string(inst, lvalue->resolved_type).data,
                        temp_type_string(inst, rvalue->resolved_type).data);
            }

            break;
        }

        case AST_Statement_Kind::ARITHMETIC_ASSIGNMENT: {

            AST_Expression *lvalue = stmt->arithmetic_assignment.lvalue;
            AST_Expression *rvalue = stmt->arithmetic_assignment.rvalue;

            if (!type_expression(inst, task, lvalue, scope, nullptr)) {
                return false;
            }

            if (!type_expression(inst, task, rvalue, scope, nullptr)) {
                return false;
            }

            if (lvalue->resolved_type != rvalue->resolved_type) {
                auto sp_id = source_range_start(inst, stmt->range_id);
                instance_fatal_error(inst, sp_id, "Mismatching types in arithmetic assignment, left: '%s', right: '%s'",
                        temp_type_string(inst, lvalue->resolved_type).data,
                        temp_type_string(inst, rvalue->resolved_type).data);
            }

            break;
        };

        case AST_Statement_Kind::CALL: {
            if (!type_expression(inst, task, stmt->call, scope, nullptr)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_expr) {

                Type *result_type = nullptr;

                assert(task->fn_decl);
                if (task->fn_decl->resolved_type) {
                    result_type = task->fn_decl->resolved_type->function.return_type;
                } else if (task->fn_decl->function.return_ts->resolved_type) {
                    result_type = task->fn_decl->function.return_ts->resolved_type;
                }

                if (!type_expression(inst, task, stmt->return_expr, scope, result_type)) {
                    return false;
                }
            }

            break;
        }

        case AST_Statement_Kind::IF: {

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {
                auto if_block = stmt->if_stmt.blocks[i];

                if (!type_expression(inst, task, if_block.cond, scope, inst->builtin_type_bool)) {
                    return false;
                }

                if (!type_statement(inst, task, if_block.then, scope)) {
                    return false;
                }
            }

            if (stmt->if_stmt.else_stmt && !type_statement(inst, task, stmt->if_stmt.else_stmt, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::WHILE: {

            AST_Expression *cond = stmt->while_stmt.cond;
            if (!type_expression(inst, task, cond, scope, inst->builtin_type_bool)) {
                return false;
            }

            if (cond->resolved_type->kind != Type_Kind::BOOLEAN) {
                auto sp_id = source_range_start(inst, cond->range_id);
                instance_fatal_error(inst, sp_id, "Expression after 'while' must be of boolean type (got: '%s')",
                                     temp_type_string(inst, cond->resolved_type).data);
            }

            AST_Statement *while_stmt = stmt->while_stmt.stmt;
            if (!type_statement(inst, task, while_stmt, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::FOR: {

            AST_Statement *init = stmt->for_stmt.init;
            if (!type_statement(inst, task, init, scope)) {
                return false;
            }

            AST_Expression *cond = stmt->for_stmt.cond;
            if (!type_expression(inst, task, cond, scope, inst->builtin_type_bool)) {
                return false;
            }

            if (cond->resolved_type->kind != Type_Kind::BOOLEAN) {
                auto sp_id = source_range_start(inst, cond->range_id);
                instance_fatal_error(inst, sp_id, "Conditional in 'for' must be of boolean type (got: '%s')",
                                     temp_type_string(inst, cond->resolved_type).data);
            }

            AST_Statement *step = stmt->for_stmt.step;
            if (!type_statement(inst, task, step, scope)) {
                return false;
            }

            AST_Statement *for_stmt = stmt->for_stmt.stmt;
            if (!type_statement(inst, task, for_stmt, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::BREAK:
        case AST_Statement_Kind::CONTINUE: {
            assert(stmt->loop_control_target);
            break;
        }

        case AST_Statement_Kind::BLOCK: {

            for (s64 i = 0; i < stmt->block.statements.count; i++) {
                if (!type_statement(inst, task, stmt->block.statements[i], scope)) {
                    return false;
                }
            }
            break;
        }
    }

    stmt->flags |= AST_TS_FLAG_TYPED;
    return true;
}

bool type_expression(Instance *inst, Type_Task *task, AST_Expression *expr, Scope *scope, Type *suggested_type)
{
    if (expr->flags & AST_EXPR_FLAG_TYPED) {
        assert(expr->resolved_type);
        return true;
    }

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            auto decl = expr->identifier->decl;
            assert(decl);

            if (!(decl->flags & AST_DECL_FLAG_TYPED)) {
                return false;
            }

            expr->resolved_type = decl->resolved_type;
            break;
        }

        case AST_Expression_Kind::BINARY: {
            if (suggested_type && suggested_type->kind == Type_Kind::BOOLEAN) {
                suggested_type = nullptr;
            }

            if (!type_expression(inst, task, expr->binary.lhs, scope, suggested_type)) {
                return false;
            }

            if (!suggested_type) {
                suggested_type = expr->binary.lhs->resolved_type;
            }

            if (!type_expression(inst, task, expr->binary.rhs, scope, suggested_type)) {
                return false;
            }

            assert(expr->binary.lhs->resolved_type == expr->binary.rhs->resolved_type);

            switch (expr->binary.op) {
                case '<':
                case '>':
                case TOK_EQ:
                case TOK_NEQ:
                case TOK_LTEQ:
                case TOK_GTEQ: {
                    expr->resolved_type = inst->builtin_type_bool;
                    break;
                }

                default: {
                    expr->resolved_type = expr->binary.lhs->resolved_type;
                    break;
                }
            }

            break;
        }

        case AST_Expression_Kind::MEMBER: {

            if (!type_expression(inst, task, expr->member.base, scope, nullptr)) {
                return false;
            }

            Type *struct_type = expr->member.base->resolved_type;
            assert(struct_type->kind == Type_Kind::STRUCT);

            AST_Declaration *field = scope_find_symbol(struct_type->structure.scope, expr->member.member_name->atom, nullptr);
            u32 index = field->variable.index;
            assert(index >= 0 && index < struct_type->structure.members.count);

            Type *mem_type = struct_type->structure.members[index].type;

            expr->resolved_type = mem_type;
            break;
        }

        case AST_Expression_Kind::CALL: {

            if (!type_expression(inst, task, expr->call.base, scope, nullptr)) {
                return false;
            }

            assert(expr->call.base->resolved_type->kind == Type_Kind::FUNCTION);
            auto fn_type = expr->call.base->resolved_type;

            for (s64 i = 0; i < expr->call.args.count; i++) {
                if (!type_expression(inst, task, expr->call.args[i], scope, fn_type->function.param_types[i])) {
                    return false;
                }

                assert(expr->call.args[i]->resolved_type == fn_type->function.param_types[i]);
            }

            expr->resolved_type = fn_type->function.return_type;
            assert(task->fn_decl);
            for (s64 i = 0; i < expr->call.args.count; i++) {
                if (expr->call.args[i]->resolved_type->kind == Type_Kind::STRUCT) {
                    darray_append(&task->fn_decl->function.temp_structs, expr->call.args[i]);
                }
            }

            if (expr->resolved_type->kind == Type_Kind::STRUCT) {
                assert(fn_type->function.return_type->kind == Type_Kind::STRUCT);
                darray_append(&task->fn_decl->function.temp_structs, expr);
            }
            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL: {

            if (suggested_type) {
                assert(suggested_type->kind == Type_Kind::INTEGER);

                s64 min = I64_MIN;

                if (suggested_type->integer.sign) {
                    s64 val = (s64)expr->integer_literal;
                    switch (suggested_type->bit_size) {
                        default: assert(false); break;
                        case 8: assert(val >= I8_MIN && val <= I8_MAX); break;
                        case 16: assert(val >= I16_MIN && val <= I16_MAX); break;
                        case 32: assert(val >= I32_MIN && val <= I32_MAX); break;
                        case 64: assert(val >= min && val <= I64_MAX); break;
                    }
                } else {
                    u64 val = (u64)expr->integer_literal;
                    switch (suggested_type->bit_size) {
                        default: assert(false); break;
                        case 8: assert(val <= I8_MAX); break;
                        case 16: assert(val <= I16_MAX); break;
                        case 32: assert(val <= I32_MAX); break;
                        case 64: assert(val <= I64_MAX); break;
                    }
                }

                expr->resolved_type = suggested_type;

            } else {
                expr->resolved_type = inst->builtin_type_s64;
            }

            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;

        case AST_Expression_Kind::BOOL_LITERAL: {
            expr->resolved_type = inst->builtin_type_bool;
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
    }

    assert(expr->resolved_type);
    expr->flags |= AST_EXPR_FLAG_TYPED;
    return true;
}

bool type_type_spec(Instance *inst, Type_Task *task, AST_Type_Spec *ts, Scope *scope)
{
    if (ts->flags & AST_TS_FLAG_TYPED) {
        assert(ts->resolved_type);
        return true;
    }

    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false); break;

        case AST_Type_Spec_Kind::IDENTIFIER: {
            auto decl = ts->identifier->decl;
            assert(decl);

            if (!decl->resolved_type) {
                return false;
            }

            ts->resolved_type = decl->resolved_type;
            break;
        }
    }

    assert(ts->resolved_type);
    ts->flags |= AST_TS_FLAG_TYPED;
    return true;
}

}
