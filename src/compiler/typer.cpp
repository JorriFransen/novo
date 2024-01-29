#include "typer.h"

#include "ast.h"
#include "instance.h"
#include "type.h"

namespace Novo {

bool type_node(Instance *inst, AST_Node *node, Scope *scope)
{
    switch (node->kind) {

        case AST_Node_Kind::INVALID: assert(false); break;

        case AST_Node_Kind::DECLARATION: {
            return type_declaration(inst, node->declaration, scope);
        }

        case AST_Node_Kind::STATEMENT: {
            return type_statement(inst, node->statement, scope);
        }

        case AST_Node_Kind::EXPRESSION: assert(false); break;

        case AST_Node_Kind::TYPE_SPEC: {
            return type_type_spec(inst, node->ts, scope);
        }
    }
}

bool type_declaration(Instance *inst, AST_Declaration *decl, Scope *scope)
{
    if (decl->flags & AST_DECL_FLAG_TYPED) {
        assert(decl->resolved_type);
        return true;
    }

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            if (decl->variable.type_spec && !type_type_spec(inst, decl->variable.type_spec, scope)) {
                return false;
            }

            if (decl->variable.init_expr && !type_expression(inst, decl->variable.init_expr, scope)) {
                return false;
            }

            Type *type = nullptr;
            if (decl->variable.type_spec && decl->variable.init_expr) {
                assert(decl->variable.type_spec->resolved_type == decl->variable.init_expr->resolved_type);
                type = decl->variable.type_spec->resolved_type;
            } else if (decl->variable.type_spec) {
                type = decl->variable.type_spec->resolved_type;
            } else {
                type = decl->variable.init_expr->resolved_type;
            }
            assert(type);

            decl->resolved_type = type;
            break;
        }

        case AST_Declaration_Kind::STRUCT_MEMBER: {
            if (!type_type_spec(inst, decl->variable.type_spec, scope)) {
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

                if (!type_declaration(inst, field, struct_scope)) {
                    return false;
                }
            }

            auto member_types = temp_array_create<Type *>(&inst->temp_allocator, fields.count);

            for (s64 i = 0; i < fields.count; i++) {
                auto field = fields[i];
                darray_append(&member_types, field->resolved_type);
            }

            decl->resolved_type = struct_type_new(inst, member_types);

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

bool type_statement(Instance *inst, AST_Statement *stmt, Scope *scope)
{
    if (stmt->flags & AST_TS_FLAG_TYPED) {
        return true;
    }

    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;
        case AST_Statement_Kind::IMPORT: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            if (!type_declaration(inst, stmt->declaration, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::ASSIGNMENT: {
            if (!type_expression(inst, stmt->assignment.lvalue, scope)) {
                return false;
            }

            if (!type_expression(inst, stmt->assignment.rvalue, scope)) {
                return false;
            }

            assert(stmt->assignment.lvalue->resolved_type == stmt->assignment.rvalue->resolved_type);
            break;
        }

        case AST_Statement_Kind::CALL: assert(false); break;

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_expr) {
                if (!type_expression(inst, stmt->return_expr, scope)) {
                    return false;
                }
            }

            break;
        }
    }

    stmt->flags |= AST_TS_FLAG_TYPED;
    return true;
}

bool type_expression(Instance *inst, AST_Expression *expr, Scope *scope)
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
            if (!type_expression(inst, expr->binary.lhs, scope)) {
                return false;
            }

            if (!type_expression(inst, expr->binary.rhs, scope)) {
                return false;
            }

            assert(expr->binary.lhs->resolved_type == expr->binary.rhs->resolved_type);
            expr->resolved_type = expr->binary.lhs->resolved_type;
            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false); break;

        case AST_Expression_Kind::CALL: {

            if (!type_expression(inst, expr->call.base, scope)) {
                return false;
            }

            assert(expr->call.base->resolved_type->kind == Type_Kind::FUNCTION);
            auto fn_type = expr->call.base->resolved_type;

            for (s64 i = 0; i < expr->call.args.count; i++) {
                if (!type_expression(inst, expr->call.args[i], scope)) {
                    return false;
                }

                assert(expr->call.args[i]->resolved_type == fn_type->function.param_types[i]);
            }

            expr->resolved_type = fn_type->function.return_type;
            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL: {
            expr->resolved_type = inst->builtin_type_s64;
            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
    }

    assert(expr->resolved_type);
    expr->flags |= AST_EXPR_FLAG_TYPED;
    return true;
}

bool type_type_spec(Instance *inst, AST_Type_Spec *ts, Scope *scope)
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
            assert(decl->resolved_type);
            ts->resolved_type = decl->resolved_type;
            break;
        }
    }

    assert(ts->resolved_type);
    ts->flags |= AST_TS_FLAG_TYPED;
    return true;
}

}
