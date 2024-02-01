#include "resolver.h"

#include <containers/darray.h>
#include <filesystem.h>
#include <logger.h>
#include <nstring.h>

#include "ast.h"
#include "atom.h"
#include "instance.h"
#include "scope.h"
#include "task.h"
#include "type.h"
#include "typer.h"

#include <assert.h>

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

    assert(false);
    return false;
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

                darray_append(&task->fn_decl->function.variables, decl); // Make sure this is only done once!
            }
            break;
        }

        case AST_Declaration_Kind::STRUCT_MEMBER: {
            if (!resolve_ts(inst, task, decl->variable.type_spec, scope)) {
                return false;
            }

            assert(!decl->variable.init_expr);

            break;
        }

        case AST_Declaration_Kind::STRUCT: {

            Scope *struct_scope = decl->structure.scope;

            for (s64 i = 0; i < decl->structure.fields.count; i++) {
                auto field = decl->structure.fields[i];
                if (!resolve_declaration(inst, task, field, struct_scope)) {
                    return false;
                }
                field->variable.index = i;
            }

            break;
        }

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

        case AST_Statement_Kind::IMPORT: {

            String import_path = string_append(&inst->temp_allocator, inst->cwd, stmt->import_path);
            assert(fs_is_file(import_path));

            if (!(fs_is_realpath(import_path))) {
                import_path = fs_realpath(&inst->ast_allocator, import_path);
            } else {
                import_path = string_copy(&inst->ast_allocator, import_path);
            }

            add_parse_task(inst, import_path);
            break;
        };

        case AST_Statement_Kind::DECLARATION: {
            if (!resolve_declaration(inst, task, stmt->declaration, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::ASSIGNMENT: {
            AST_Expression *lvalue = stmt->assignment.lvalue;

            if (!resolve_expression(inst, task, lvalue, scope)) {
                return false;
            }

            if (lvalue->kind == AST_Expression_Kind::IDENTIFIER &&
                lvalue->identifier->decl->kind == AST_Declaration_Kind::VARIABLE) {
                lvalue->identifier->decl->flags |= AST_DECL_FLAG_STORAGE_REQUIRED;
            }

            if (!resolve_expression(inst, task, stmt->assignment.rvalue, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::CALL: {
            if (!resolve_expression(inst, task, stmt->call, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::RETURN: {

            if (stmt->return_expr) {
                if (!resolve_expression(inst, task, stmt->return_expr, scope)) {
                    return false;
                }
            }

            break;
        }

        case AST_Statement_Kind::IF: {

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {

                auto if_block = stmt->if_stmt.blocks[i];

                if (!resolve_expression(inst, task, if_block.cond, scope)) {
                    return false;
                }

                if (!resolve_statement(inst, task, if_block.then, scope)) {
                    return false;
                }
            }

            if (stmt->if_stmt.else_stmt && !resolve_statement(inst, task, stmt->if_stmt.else_stmt, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::WHILE: {

            if (!resolve_expression(inst, task, stmt->while_stmt.cond, scope)) {
                return false;
            }

            if (!resolve_statement(inst, task, stmt->while_stmt.stmt, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::FOR: {

            Scope *for_scope = stmt->for_stmt.scope;

            if (!resolve_statement(inst, task, stmt->for_stmt.init, for_scope)) {
                return false;
            }

            if (!resolve_expression(inst, task, stmt->for_stmt.cond, for_scope)) {
                return false;
            }

            if (!resolve_statement(inst, task, stmt->for_stmt.step, for_scope)) {
                return false;
            }

            if (!resolve_statement(inst, task, stmt->for_stmt.stmt, for_scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::BLOCK: {

            for (s64 i = 0; i < stmt->block.statements.count; i++) {
                AST_Statement *block_stmt = stmt->block.statements[i];

                if (!resolve_statement(inst, task, block_stmt, scope)) {
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

        case AST_Expression_Kind::MEMBER: {
            if (!resolve_expression(inst, task, expr->member.base, scope)) {
                return false;
            }

            if (!(expr->member.base->flags & AST_EXPR_FLAG_TYPED)) {

                Type_Task type_task = {
                    .node = ast_node(expr->member.base),
                    .scope = scope,
                    .fn_decl = task->fn_decl,
                };
                if (!type_expression(inst, &type_task, expr->member.base, scope)) {
                    return false;
                }
            }

            Type *struct_type = expr->member.base->resolved_type;
            assert(struct_type->kind == Type_Kind::STRUCT);

            Scope *struct_scope = struct_type->structure.scope;

            if (!resolve_identifier(inst, task, expr->member.member_name, struct_scope)) {
                return false;
            }

            break;
        }

        case AST_Expression_Kind::CALL: {

            if (!resolve_expression(inst, task, expr->call.base, scope)) {
                return false;
            }

            AST_Declaration *callee_decl = nullptr;
            if (expr->call.base->kind == AST_Expression_Kind::IDENTIFIER) {
                auto callee_decl = expr->call.base->identifier->decl;
                assert(callee_decl);

                assert(task->fn_decl);
            }

            for (s64 i = 0; i < expr->call.args.count; i++) {

                if (!resolve_expression(inst, task, expr->call.args[i], scope)) {
                    return false;
                }
            }

            // success
            if (callee_decl) {
                darray_append(&task->fn_decl->function.wait_for_bytecode, ast_node(callee_decl));
            }
            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::BOOL_LITERAL: break;

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
    task->waiting_for = nullptr;

    if (ident->decl) return true;

    if (AST_Declaration *found_decl = scope_find_symbol(scope, ident->atom)) {
        ident->decl = found_decl;
        return true;
    }

    task->waiting_for = ident;

    auto name = atom_string(ident->atom);
    log_trace("Waiting for undeclared identifier: '%s'", name.data);
    return false;
}

}
