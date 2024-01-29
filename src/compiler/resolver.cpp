#include "resolver.h"

#include <logger.h>

#include "ast.h"
#include "parser.h"
#include "scope.h"
#include "task.h"

namespace Novo {

bool resolve_node(Instance *inst, Resolve_Task *task, AST_Node *node, Scope *scope)
{
    switch (node->kind) {

        case AST_Node_Kind::INVALID: assert(false); break;

        case AST_Node_Kind::DECLARATION: {
            return resolve_declaration(inst, task, node->declaration, scope);
        }

        case AST_Node_Kind::STATEMENT: {
            return resolve_statement(inst, task, node->statement, scope);
        }

        case AST_Node_Kind::EXPRESSION: assert(false); break;

        case AST_Node_Kind::TYPE_SPEC: {
            return resolve_ts(inst, task, node->ts, scope);
        }
    }
}

bool resolve_declaration(Instance *inst, Resolve_Task *task, AST_Declaration *decl, Scope *scope)
{
    if (decl->flags & AST_DECL_FLAG_RESOLVED) {
        return true;
    }

    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {
            if (decl->variable.type_spec && !resolve_ts(inst, task, decl->variable.type_spec, scope)) {
                return false;
            }

            if (decl->variable.init_expr && !resolve_expression(inst, task, decl->variable.init_expr, scope)) {
                return false;
            }

            if (decl->flags & AST_DECL_FLAG_PARAM) {
                assert(decl->variable.type_spec);
            } else {
                assert(decl->variable.type_spec || decl->variable.init_expr);
                assert(task->fn_decl);
                darray_append(&task->fn_decl->function.variables, decl);
            }
            break;
        }

        case AST_Declaration_Kind::STRUCT_MEMBER: assert(false); break;
        case AST_Declaration_Kind::STRUCT: assert(false); break;

        case AST_Declaration_Kind::FUNCTION: {

            for (s64 i = 0; i < decl->function.params.count; i++) {
                if (!(decl->function.params[i]->flags & AST_DECL_FLAG_RESOLVED)) {
                    return false;
                }
            }

            if (decl->function.return_ts && !(decl->function.return_ts->flags & AST_TS_FLAG_RESOLVED)) {
                return false;
            }

            for (s64 i = 0; i < decl->function.body.count; i++) {
                if (!(decl->function.body[i]->flags & AST_STMT_FLAG_RESOLVED)) {
                    return false;
                }
            }

            break;
        }

        case AST_Declaration_Kind::BUILTIN_TYPE: assert(false); break;
    }

    decl->flags |= AST_DECL_FLAG_RESOLVED;
    return true;
}

bool resolve_statement(Instance *inst, Resolve_Task *task, AST_Statement *stmt, Scope *scope)
{
    if (stmt->flags & AST_STMT_FLAG_RESOLVED) {
        return true;
    }

    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;
        case AST_Statement_Kind::IMPORT: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            if (!resolve_declaration(inst, task, stmt->declaration, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::ASSIGNMENT: {
            if (!resolve_expression(inst, task, stmt->assignment.lvalue, scope)) {
                return false;
            }

            if (!resolve_expression(inst, task, stmt->assignment.rvalue, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::CALL: assert(false); break;

        case AST_Statement_Kind::RETURN: {

            if (stmt->return_expr) {
                if (!resolve_expression(inst, task, stmt->return_expr, scope)) {
                    return false;
                }
            }

            break;
        }
    }

    stmt->flags |= AST_STMT_FLAG_RESOLVED;
    return true;
}

bool resolve_expression(Instance *inst, Resolve_Task *task, AST_Expression *expr, Scope *scope)
{
    if (expr->flags & AST_EXPR_FLAG_RESOLVED) {
        return true;
    }

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            if (!resolve_identifier(inst, task, expr->identifier, scope)) {
                return false;
            }

            break;
        }

        case AST_Expression_Kind::BINARY: {
            if (!resolve_expression(inst, task, expr->binary.lhs, scope)) {
                return false;
            }

            if (!resolve_expression(inst, task, expr->binary.rhs, scope)) {
                return false;
            }

            break;
        }

        case AST_Expression_Kind::MEMBER: assert(false); break;

        case AST_Expression_Kind::CALL: {

            if (!resolve_expression(inst, task, expr->call.base, scope)) {
                return false;
            }

            if (expr->call.base->kind == AST_Expression_Kind::IDENTIFIER) {
                auto decl = expr->call.base->identifier->decl;
                assert(decl);

                assert(task->fn_decl);

                darray_append(&task->fn_decl->function.wait_for_bytecode, ast_node(decl));
            }

            for (s64 i = 0; i < expr->call.args.count; i++) {

                if (!resolve_expression(inst, task, expr->call.args[i], scope)) {
                    return false;
                }
            }

            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL: break;

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;

    }

    expr->flags |= AST_EXPR_FLAG_RESOLVED;
    return true;
}

bool resolve_ts(Instance *inst, Resolve_Task *task, AST_Type_Spec *ts, Scope *scope)
{
    if (ts->flags & AST_TS_FLAG_RESOLVED) {
        return true;
    }

    bool result = false;

    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false); break;
        case AST_Type_Spec_Kind::IDENTIFIER: {
             result = resolve_identifier(inst, task, ts->identifier, scope);
             break;
        }
    }

    if (result) {
        ts->flags |= AST_TS_FLAG_RESOLVED;
    }

    return result;
}

bool resolve_identifier(Instance *inst, Resolve_Task *task, AST_Identifier *ident, Scope *scope)
{
    if (ident->decl) return true;

    if (AST_Declaration *found_decl = scope_find_symbol(scope, ident->atom)) {
        ident->decl = found_decl;
        return true;
    }

    auto name = atom_string(ident->atom);
    log_trace("Waiting for undeclared identifier: '%s'", name.data);
    return false;
}

}
