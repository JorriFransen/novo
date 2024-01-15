#include "resolve.h"

#include "ast.h"
#include "instance.h"
#include "logger.h"
#include "scope.h"
#include "source_pos.h"

#include <cassert>

namespace Novo {

bool resolve_declaration(Instance *instance, AST_Declaration *decl, Scope *scope)
{
    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            if (decl->variable.ts) {
                if (!resolve_ts(instance, decl->variable.ts, scope)) {
                    return false;
                }
            }

            if (decl->variable.init_expr) {
                if (!resolve_expression(instance, decl->variable.init_expr, scope)) {
                    return false;
                }
            }

            return true;
        }

        case AST_Declaration_Kind::FUNCTION: {

            auto function_scope = decl->function.scope;

            for (s64 i = 0; i < decl->function.params.count; i++) {
                if (!resolve_declaration(instance, decl->function.params[i], function_scope)) {
                    return false;
                }
            }

            if (decl->function.return_ts) {
                if (!resolve_ts(instance, decl->function.return_ts, function_scope)) {
                    return false;
                }
            }

            for (s64 i = 0; i < decl->function.body.count; i++) {
                if (!resolve_statement(instance, decl->function.body[i], function_scope)) {
                    return false;
                }
            }

            return true;
        }
    }

    assert(false);
    return false;
}

bool resolve_statement(Instance *instance, AST_Statement *stmt, Scope *scope)
{
    switch (stmt->kind) {
        case AST_Statement_Kind::INVALID: assert(false); break;

        case AST_Statement_Kind::IMPORT: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            return resolve_declaration(instance, stmt->declaration, scope);
        }

        case AST_Statement_Kind::CALL: {
            return resolve_expression(instance, stmt->call, scope);
        }

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_expr) {
                return resolve_expression(instance, stmt->return_expr, scope);
            }
            return true;
        }
    }

    assert(false);
    return false;
}

bool resolve_expression(Instance *instance, AST_Expression *expr, Scope *scope)
{
    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            return resolve_ident(instance, expr->identifier, scope);
        }

        case AST_Expression_Kind::BINARY: assert(false); break;

        case AST_Expression_Kind::CALL: {

            if (!resolve_expression(instance, expr->call.base, scope)) {
                return false;
            }

            for (s64 i = 0; i < expr->call.args.count; i++) {
                if (!resolve_expression(instance, expr->call.args[i], scope)) {
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

bool resolve_ts(Instance *instance, AST_Type_Spec *ts, Scope *scope)
{
    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false); break;

        case AST_Type_Spec_Kind::IDENTIFIER: {
            return resolve_ident(instance, ts->identifier, scope);
            break;
        }
    }

    assert(false);
    return false;
}

bool resolve_ident(Instance *instance, AST_Identifier *ident, Scope *scope)
{
    if (ident->decl) return true;

    if (auto found_decl = scope_find_symbol(scope, ident->atom)) {
        ident->decl = found_decl;
        return true;
    }

    if (ident->atom == atom_get("int")) {
        return true;
    }

    auto start_id = source_range_start(instance, ident->range_id);
    auto name = atom_string(ident->atom);
    instance_error(instance, start_id, "Reference to undeclared identifier: '%s'", name.data);
    log_trace("Waiting for undeclared identifier: '%s'\n", name.data);

    return false;
}

}
