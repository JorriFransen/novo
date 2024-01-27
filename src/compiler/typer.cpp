#include "typer.h"

#include <containers/darray.h>
#include <nstring.h>

#include "ast.h"
#include "instance.h"
#include "source_pos.h"
#include "task.h"
#include "type.h"

#include <cassert>

namespace Novo {

bool type_declaration(Instance *inst, Task *task, AST_Declaration *decl, Scope *scope)
{
    task->type.waiting_for = nullptr;

    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false); break;
        case AST_Declaration_Kind::BUILTIN_TYPE: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            Type *type = nullptr;
            if (decl->variable.type_spec) {
                if (!type_type_spec(inst, task, decl->variable.type_spec, scope)) {
                    return false;
                }

                type = decl->variable.type_spec->resolved_type;
            }

            if (decl->variable.init_expr) {
                if (!type_expression(inst, task, decl->variable.init_expr, scope)) {
                    return false;
                }

                if (!type) {
                    type = decl->variable.init_expr->resolved_type;
                }
            }

            assert(type);
            assert(decl->variable.type_spec || decl->variable.init_expr);

            if (decl->variable.type_spec && decl->variable.init_expr) {
                if (decl->variable.type_spec->resolved_type != decl->variable.init_expr->resolved_type) {

                    auto sp_id = source_range_start(inst, decl->variable.init_expr->range_id);
                    instance_fatal_error(inst, sp_id, "Mismatching type in variable declaration, expected: '%s', got: '%s'",
                            temp_type_string(inst, decl->variable.type_spec->resolved_type).data,
                            temp_type_string(inst, decl->variable.init_expr->resolved_type).data);
                }

            }

            decl->resolved_type = type;

            return true;
        }

        case AST_Declaration_Kind::STRUCT_MEMBER: {

            if (!type_type_spec(inst, task, decl->variable.type_spec, scope)) {
                return false;
            }

            if (decl->variable.init_expr) {
                if (!type_expression(inst, task, decl->variable.init_expr, scope)) {
                    return false;
                }

                assert(decl->variable.type_spec->resolved_type == decl->variable.init_expr->resolved_type);
            }

            decl->resolved_type = decl->variable.type_spec->resolved_type;

            return true;
        }

        case AST_Declaration_Kind::STRUCT: {

            auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);

            for (s64 i = 0; i < decl->structure.fields.count; i++) {

                auto field = decl->structure.fields[i];
                auto struct_scope = decl->structure.scope;

                if (!type_declaration(inst, task, field, struct_scope)) {
                    return false;
                }

                field->variable.index = i;
            }

            auto member_types = temp_array_create<Type *>(&inst->temp_allocator, decl->structure.fields.count);
            for (s64 i = 0; i < decl->structure.fields.count; i++) {
                darray_append(&member_types, decl->structure.fields[i]->resolved_type);
            }

            decl->resolved_type = struct_type_new(inst, member_types);
            temp_allocator_reset(&inst->temp_allocator_data, mark);

            return true;
        }

        case AST_Declaration_Kind::FUNCTION: {

            auto func_scope = decl->function.scope;

            for (s64 i = 0; i < decl->function.params.count; i++) {
                if (!type_declaration(inst, task, decl->function.params[i], func_scope)) {
                    return false;
                }
            }

            Type *return_type = nullptr;

            if (decl->function.return_ts) {

                if (!type_type_spec(inst, task, decl->function.return_ts, func_scope)) {
                    return false;
                }

                assert(decl->function.return_ts->resolved_type);
                return_type = decl->function.return_ts->resolved_type;
            } else {
                return_type = inst->builtin_type_void;
            }

            assert(return_type);

            auto param_types = temp_array_create<Type *>(&inst->temp_allocator, decl->function.params.count);
            for (s64 i = 0; i < decl->function.params.count; i++) {
                assert(decl->function.params[i]->resolved_type);
                darray_append(&param_types, decl->function.params[i]->resolved_type);
            }

            decl->resolved_type = function_type_get(inst, param_types, return_type);

            assert(task->type.current_function_type == nullptr);
            task->type.current_function_type = decl->resolved_type;

            for (s64 i = 0; i < decl->function.body.count; i++) {
                if (!type_statement(inst, task, decl->function.body[i], func_scope)) {
                    task->type.current_function_type = nullptr;
                    return false;
                }
            }

            task->type.current_function_type = nullptr;

            return true;
        }
    }

    assert(false);
    return false;
}

bool type_statement(Instance *inst, Task *task, AST_Statement *stmt, Scope *scope)
{
    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;
        case AST_Statement_Kind::IMPORT: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            return type_declaration(inst, task, stmt->declaration, scope);
        }

        case AST_Statement_Kind::ASSIGNMENT: {
            if (!type_expression(inst, task, stmt->assignment.lvalue, scope)) {
                return false;
            }

            if (!type_expression(inst, task, stmt->assignment.rvalue, scope)) {
                return false;
            }

            auto ltype = stmt->assignment.lvalue->resolved_type;
            auto rtype = stmt->assignment.rvalue->resolved_type;

            if (ltype != rtype) {

                auto sp_id = source_range_start(inst, stmt->range_id);
                instance_fatal_error(inst, sp_id, "Mismatching types in assignment, expected: '%s', got: '%s'",
                        temp_type_string(inst, ltype).data,
                        temp_type_string(inst, rtype).data);
            }

            return true;
        }

        case AST_Statement_Kind::CALL: {
            return type_expression(inst, task, stmt->call, scope);
        }

        case AST_Statement_Kind::RETURN: {

            if (stmt->return_expr) {

                if (!type_expression(inst, task, stmt->return_expr, scope)) {
                    return false;
                }

                assert(task->type.current_function_type);
                auto fn_type = task->type.current_function_type;
                assert(fn_type->kind == Type_Kind::FUNCTION);

                if (fn_type->function.return_type != stmt->return_expr->resolved_type) {
                    auto sp_id = source_range_start(inst, stmt->return_expr->range_id);
                    instance_fatal_error(inst, sp_id, "Mismatching return type, expected: '%s', got: '%s'",
                            temp_type_string(inst, fn_type->function.return_type).data,
                            temp_type_string(inst, stmt->return_expr->resolved_type).data);
                }

                return true;

            } else {
                return true;
            }
        }

    }

    assert(false);
    return false;
}

bool type_expression(Instance *inst, Task *task, AST_Expression *expr, Scope *scope)
{
    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            assert(expr->identifier->decl);

            if (!expr->identifier->decl->resolved_type) {
                task->type.waiting_for = expr->identifier->decl;
                return false;
            }

            expr->resolved_type = expr->identifier->decl->resolved_type;
            return true;
        }

        case AST_Expression_Kind::BINARY: {

            if (!type_expression(inst, task, expr->binary.lhs, scope)) {
                return false;
            }

            if (!type_expression(inst, task, expr->binary.rhs, scope)) {
                return false;
            }

            auto lhs_type = expr->binary.lhs->resolved_type;
            auto rhs_type = expr->binary.rhs->resolved_type;

            if (lhs_type != rhs_type) {
                auto sp_id = source_range_start(inst, expr->range_id);
                instance_fatal_error(inst, sp_id, "Mismatching types in binary expression, left: '%s', right: '%s'",
                        temp_type_string(inst, lhs_type).data, temp_type_string(inst, rhs_type).data);

            }

            expr->resolved_type = lhs_type;
            return true;
        }

        case AST_Expression_Kind::CALL: {

            auto base = expr->call.base;
            if (!type_expression(inst, task, base, scope)) {
                return false;
            }

            auto fn_type = base->resolved_type;
            if (fn_type->kind != Type_Kind::FUNCTION) {
                auto sp_id = source_range_start(inst, base->range_id);
                instance_fatal_error(inst, sp_id, "Base expression of call is not of function type");
            }

            for (s64 i = 0; i < expr->call.args.count; i++) {

                auto arg = expr->call.args[i];

                if (!type_expression(inst, task, arg, scope)) {
                    return false;
                }

                auto param_type = fn_type->function.param_types[i];

                if (arg->resolved_type != param_type) {
                    auto sp_id = source_range_start(inst, arg->range_id);
                    instance_fatal_error(inst, sp_id, "Mismatching type for argument %d, expected: '%s', got: '%s'",
                            i + 1,
                            temp_type_string(inst, param_type).data,
                            temp_type_string(inst, arg->resolved_type).data);
                }

            }

            expr->resolved_type = fn_type->function.return_type;

            return true;
        }

        case AST_Expression_Kind::INTEGER_LITERAL: {
            expr->resolved_type = inst->builtin_type_s64;
            return true;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
    }

    assert(false);
}

bool type_type_spec(Instance *inst, Task *task, AST_Type_Spec *ts, Scope *scope)
{
    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false); break;

        case AST_Type_Spec_Kind::IDENTIFIER: {
            assert(ts->identifier->decl);
            assert(ts->identifier->decl->resolved_type);

            ts->resolved_type = ts->identifier->decl->resolved_type;

            return true;
        }
    }

    assert(false);
    return false;
}

}
