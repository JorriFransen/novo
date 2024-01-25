#include "resolver.h"

#include <containers/darray.h>
#include <nstring.h>

#include "ast.h"
#include "atom.h"
#include "logger.h"
#include "scope.h"
#include "task.h"

#include <cassert>

namespace Novo {

bool resolve_declaration(Instance *instance, Task *task, AST_Declaration *decl, Scope *scope, DArray<AST_Declaration *> *variables)
{
    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false); break;
        case AST_Declaration_Kind::BUILTIN_TYPE: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            if (decl->variable.type_spec) {
                if (!resolve_ts(instance, task, decl->variable.type_spec, scope)) {
                    return false;
                }
            }

            if (decl->variable.init_expr) {
                if (!resolve_expression(instance, task, decl->variable.init_expr, scope)) {
                    return false;
                }
            }

            if (variables) {
                assert(scope->kind == Scope_Kind::FUNCTION_LOCAL);
                assert(!(decl->flags & AST_DECL_FLAG_PARAM));
                darray_append(variables, decl);
            }

            return true;
        }

        case AST_Declaration_Kind::STRUCT_MEMBER: {
            assert(false);
            return false;
        }

        case AST_Declaration_Kind::STRUCT: {
            assert(false);
            return false;
        }

        case AST_Declaration_Kind::FUNCTION: {

            assert(variables == nullptr);

            assert(scope != decl->function.scope);
            auto fn_scope = decl->function.scope;

            for (s64 i = 0; i < decl->function.params.count; i++) {
                if (!resolve_declaration(instance, task, decl->function.params[i], fn_scope, nullptr)) {
                    return false;
                }
            }

            if (decl->function.return_ts) {
                if (!resolve_ts(instance, task, decl->function.return_ts, scope)) {
                    return false;
                }
            }

            for (s64 i = 0; i < decl->function.body.count; i++) {
                if (!resolve_statement(instance, task, decl->function.body[i], fn_scope, &decl->function.variables)) {
                    return false;
                }
            }

            return true;
        }
    }

    assert(false);
    return false;
}

bool resolve_statement(Instance *instance, Task *task, AST_Statement *stmt, Scope *scope, DArray<AST_Declaration*> *variables)
{
    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false); break;

        case AST_Statement_Kind::IMPORT: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            return resolve_declaration(instance, task, stmt->declaration, scope, variables);
        }

        case AST_Statement_Kind::ASSIGNMENT: {
            if (!resolve_expression(instance, task, stmt->assignment.lvalue, scope)) {
                return false;
            }

            if (!resolve_expression(instance, task, stmt->assignment.rvalue, scope)) {
                return false;
            }

            return true;
        }

        case AST_Statement_Kind::CALL: {
            return resolve_expression(instance, task, stmt->call, scope);
        }

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_expr) {
                return resolve_expression(instance, task, stmt->return_expr, scope);
            }
            return true;
        }
    }

    assert(false);
    return false;
}

bool resolve_expression(Instance *instance, Task *task, AST_Expression *expr, Scope *scope)
{
    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            return resolve_ident(instance, task, expr->identifier, scope);
        }

        case AST_Expression_Kind::BINARY: {

            if (!resolve_expression(instance, task, expr->binary.lhs, scope)) {
                return false;
            }

            if (!resolve_expression(instance, task, expr->binary.rhs, scope)) {
                return false;
            }

            return true;
        }

        case AST_Expression_Kind::CALL: {

            if (!resolve_expression(instance, task, expr->call.base, scope)) {
                return false;
            }

            for (s64 i = 0; i < expr->call.args.count; i++) {
                if (!resolve_expression(instance, task, expr->call.args[i], scope)) {
                    return false;
                }
            }

            return true;
        }

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::REAL_LITERAL:
        case AST_Expression_Kind::CHAR_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL: {
            return true;
        }
    }

    assert(false);
    return false;
}

bool resolve_ts(Instance *instance, Task *task, AST_Type_Spec *ts, Scope *scope)
{
    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false); break;

        case AST_Type_Spec_Kind::IDENTIFIER: {
            return resolve_ident(instance, task, ts->identifier, scope);
            break;
        }
    }

    assert(false);
    return false;
}

bool resolve_ident(Instance *instance, Task *task, AST_Identifier *ident, Scope *scope)
{
    task->resolve.waiting_for = nullptr;

    if (ident->decl) return true;

    if (auto found_decl = scope_find_symbol(scope, ident->atom)) {
        ident->decl = found_decl;
        return true;
    }

    task->resolve.waiting_for = ident;
    auto name = atom_string(ident->atom);
    log_trace("Waiting for undeclared identifier: '%s'", name.data);
    return false;
}

}
