#include <containers/darray.h>
#include <containers/stack.h>
#include <defines.h>
#include <filesystem.h>
#include <logger.h>
#include <nstring.h>

#include "ast.h"
#include "atom.h"
#include "instance.h"
#include "resolver.h"
#include "scope.h"
#include "source_pos.h"
#include "task.h"
#include "token.h"
#include "type.h"
#include "typer.h"

#include <assert.h>

namespace Novo {

bool resolve_node(Instance* inst, Resolve_Task* task, AST_Node* node, Scope* scope)
{
    switch (node->kind) {

        case AST_Node_Kind::INVALID: assert(false); break;

        case AST_Node_Kind::DECLARATION: {
            return resolve_declaration(inst, task, node->declaration, scope);
        }

        case AST_Node_Kind::STATEMENT: {
            return resolve_statement(inst, task, node->statement, scope);
        }

        case AST_Node_Kind::EXPRESSION: {
            return resolve_expression(inst, task, node->expression, scope);
        }

        case AST_Node_Kind::TYPE_SPEC: {
            return resolve_ts(inst, task, node->ts, scope);
        }

        case AST_Node_Kind::IDENTIFIER: {
            return resolve_identifier(inst, task, node->identifier, scope);
        }
    }

    assert(false);
    return false;
}

bool resolve_declaration(Instance* inst, Resolve_Task* task, AST_Declaration* decl, Scope* scope)
{
    if (decl->flags & AST_DECL_FLAG_RESOLVED) {
        return true;
    }

    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            if (decl->variable.type_spec) {
                if (!resolve_ts(inst, task, decl->variable.type_spec, scope)) {
                    return false;
                }
            }

            if (decl->variable.init_expr && !resolve_expression(inst, task, decl->variable.init_expr, scope)) {
                return false;
            }

            if (decl->flags & AST_DECL_FLAG_PARAM) {
                assert(decl->variable.type_spec);

            } else if (decl->flags & AST_DECL_FLAG_GLOBAL) {
                assert(decl->variable.type_spec || decl->variable.init_expr);

            } else {
                assert(decl->variable.type_spec || decl->variable.init_expr);
                assert(task->fn_decl);

                darray_append_unique(&task->fn_decl->function.variables, decl);
                add_type_task(inst, ast_node(decl), infer_node(), scope, task->fn_decl, task->bytecode_deps);
            }
            break;
        }

        case AST_Declaration_Kind::CONSTANT: {

            if (decl->constant.type_spec) {
                if (!resolve_ts(inst, task, decl->constant.type_spec, scope)) {
                    return false;
                }
            }

            if (!resolve_expression(inst, task, decl->constant.value, scope)) {
                return false;
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

            Scope* struct_scope = decl->structure.scope;

            for (s64 i = 0; i < decl->structure.members.count; i++) {
                auto field = decl->structure.members[i];
                if (!resolve_declaration(inst, task, field, struct_scope)) {
                    return false;
                }
                field->variable.index = i;
            }

            break;
        }

        case AST_Declaration_Kind::ENUM_MEMBER: {
            if (decl->enum_member.value_expr) {
                if (!resolve_expression(inst, task, decl->enum_member.value_expr, scope)) {
                    return false;
                }
            }
            break;
        }

        case AST_Declaration_Kind::ENUM: {

            Scope* enum_scope = decl->enumeration.scope;

            if (decl->enumeration.strict_ts) {
                if (!resolve_ts(inst, task, decl->enumeration.strict_ts, scope)) {
                    return false;
                }
            }

            for (s64 i = 0; i < decl->enumeration.members.count; i++) {
                AST_Declaration* member = decl->enumeration.members[i];
                if (!resolve_declaration(inst, task, member, enum_scope)) {
                    return false;
                }
            }

            Infer_Node infer_from;
            if (decl->enumeration.strict_ts) {
                infer_from = infer_node(decl->enumeration.strict_ts);
            } else {
                infer_from = infer_node(inst->builtin_type_s64);
            }

            // Only do this when all members are resolved, to avoid creating duplicate tasks
            for (s64 i = 0; i < decl->enumeration.members.count; i++) {
                AST_Declaration* member = decl->enumeration.members[i];
                add_type_task(inst, ast_node(member), infer_from, enum_scope, task->fn_decl, task->bytecode_deps);
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

bool resolve_statement(Instance* inst, Resolve_Task* task, AST_Statement* stmt, Scope* scope)
{
    if (stmt->flags & AST_STMT_FLAG_RESOLVED) {
        return true;
    }

    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;

        case AST_Statement_Kind::IMPORT: {

            String_Ref import_path = stmt->import_path;
            assert(fs_is_file(import_path));

            if (!(fs_is_realpath(import_path))) {
                import_path = fs_realpath(&inst->ast_allocator, import_path);
            } else {
                import_path = string_copy(&inst->ast_allocator, import_path);
            }

            Atom path_atom = atom_get(import_path);

            bool imported = false;
            for (s64 i = 0; i < inst->imported_files.count; i++) {
                if (inst->imported_files[i].name == path_atom) {
                    imported = true;
                    break;
                }
            }

            if (!imported) {
                add_parse_task(inst, path_atom);
            }
            break;
        };

        case AST_Statement_Kind::DECLARATION: {
            if (!resolve_declaration(inst, task, stmt->declaration, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::ASSIGNMENT: {
            AST_Expression* lvalue = stmt->assignment.lvalue;

            if (!resolve_expression(inst, task, lvalue, scope)) {
                return false;
            }

            if (lvalue->flags & AST_EXPR_FLAG_CONST) {
                Source_Pos pos = source_pos(inst, lvalue); // TODO: Report position of '='
                instance_fatal_error(inst, pos, "Attempting to assign to a constant");
            }

            if (lvalue->kind == AST_Expression_Kind::IDENTIFIER &&
                lvalue->identifier->decl->kind == AST_Declaration_Kind::VARIABLE &&
                lvalue->identifier->decl->flags & AST_DECL_FLAG_PARAM) {
                lvalue->identifier->decl->flags |= AST_DECL_FLAG_PARAMETER_STORAGE_REQUIRED;
            }

            if (!resolve_expression(inst, task, stmt->assignment.rvalue, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::ARITHMETIC_ASSIGNMENT: {

            AST_Expression* lvalue = stmt->arithmetic_assignment.lvalue;

            if (!resolve_expression(inst, task, lvalue, scope)) {
                return false;
            }

            if (lvalue->flags & AST_EXPR_FLAG_CONST) {
                Source_Pos pos = source_pos(inst, lvalue); // TODO: Report position of operator
                instance_fatal_error(inst, pos, "Attempting to assign to a constant");
            }

            if (lvalue->kind == AST_Expression_Kind::IDENTIFIER &&
                lvalue->identifier->decl->kind == AST_Declaration_Kind::VARIABLE &&
                lvalue->identifier->decl->flags & AST_DECL_FLAG_PARAM) {

                lvalue->identifier->decl->flags |= AST_DECL_FLAG_PARAMETER_STORAGE_REQUIRED;
            }

            assert(is_binary_arithmetic_op(stmt->arithmetic_assignment.op));

            if (!resolve_expression(inst, task, stmt->arithmetic_assignment.rvalue, scope)) {
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

            stack_push(&task->loop_control_stack, stmt);
            bool while_result = resolve_statement(inst, task, stmt->while_stmt.stmt, scope);
            stack_pop(&task->loop_control_stack);

            if (!while_result) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::FOR: {

            Scope* for_scope = stmt->for_stmt.scope;

            if (!resolve_statement(inst, task, stmt->for_stmt.init, for_scope)) {
                return false;
            }

            if (!resolve_expression(inst, task, stmt->for_stmt.cond, for_scope)) {
                return false;
            }

            if (!resolve_statement(inst, task, stmt->for_stmt.step, for_scope)) {
                return false;
            }

            stack_push(&task->loop_control_stack, stmt);
            bool for_stmt_result = resolve_statement(inst, task, stmt->for_stmt.stmt, for_scope);
            stack_pop(&task->loop_control_stack);

            if (!for_stmt_result) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::BREAK:
        case AST_Statement_Kind::CONTINUE: {
            assert(stack_count(&task->loop_control_stack));
            stmt->loop_control_target = stack_top(&task->loop_control_stack);
            break;
        }

        case AST_Statement_Kind::RUN: {
            AST_Expression* run_expr = stmt->run.expression;
            assert(run_expr->kind == AST_Expression_Kind::CALL);

            if (!resolve_expression(inst, task, run_expr, scope)) {
                return false;
            }

            for (s64 i = 0; i < run_expr->call.args.count; i++) {
                AST_Expression* arg_expr = run_expr->call.args[i];

                if (!(arg_expr->flags & AST_EXPR_FLAG_CONST)) {
                    instance_fatal_error(inst, source_pos(inst, arg_expr), "Argument expression inside #run must be a constant.");
                }
            }

            break;
        }

        case AST_Statement_Kind::INSERT: {

            AST_Expression* insert_expr = stmt->insert.expression;
            assert(insert_expr->kind == AST_Expression_Kind::CALL);

            if (!resolve_expression(inst, task, insert_expr, scope)) {
                return false;
            }

            for (s64 i = 0; i < insert_expr->call.args.count; i++) {
                AST_Expression* arg_expr = insert_expr->call.args[i];

                if (!(arg_expr->flags & AST_EXPR_FLAG_CONST)) {
                    instance_fatal_error(inst, source_pos(inst, arg_expr), "Argument expression inside #insert must be a constant.");
                }

            }

            stmt->insert.fn_decl = task->fn_decl;
            break;
        }

        case AST_Statement_Kind::BLOCK: {

            for (s64 i = 0; i < stmt->block.statements.count; i++) {
                AST_Statement* block_stmt = stmt->block.statements[i];

                if (!resolve_statement(inst, task, block_stmt, stmt->block.scope)) {
                    return false;
                }

            }

            break;
        }

        case AST_Statement_Kind::ASSERT: {

            if (!resolve_expression(inst, task, stmt->assert_stmt.cond, scope)) {
                return false;
            }

            if (stmt->assert_stmt.message) {
                if (!resolve_expression(inst, task, stmt->assert_stmt.message, scope)) {
                    return false;
                }
            }

            break;
        }
    }

    stmt->flags |= AST_STMT_FLAG_RESOLVED;

    return true;
}

bool resolve_expression(Instance* inst, Resolve_Task* task, AST_Expression* expr, Scope* scope)
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

            assert(expr->identifier->decl);
            if (expr->identifier->decl->kind == AST_Declaration_Kind::CONSTANT ||
                expr->identifier->decl->kind == AST_Declaration_Kind::ENUM_MEMBER) {
                expr->flags |= AST_EXPR_FLAG_CONST;
            }

            if (expr->identifier->decl->flags & AST_DECL_FLAG_GLOBAL) {
                assert(task->bytecode_deps);
                darray_append_unique(task->bytecode_deps, ast_node(expr->identifier->decl));
            }

            if (expr->identifier->decl->kind == AST_Declaration_Kind::VARIABLE) {
                expr->flags |= AST_EXPR_FLAG_LVALUE;
            }

            break;
        }

        case AST_Expression_Kind::UNARY: {
            AST_Expression* operand = expr->unary.operand;

            if (!resolve_expression(inst, task, operand, scope)) {
                return false;
            }


            if (operand->flags & AST_EXPR_FLAG_CONST) {
                expr->flags |= AST_EXPR_FLAG_CONST;
            }

            break;
        }

        case AST_Expression_Kind::BINARY: {
            AST_Expression* lhs = expr->binary.lhs;
            AST_Expression* rhs = expr->binary.rhs;

            if (!resolve_expression(inst, task, lhs, scope)) {
                return false;
            }

            if (!resolve_expression(inst, task, rhs, scope)) {
                return false;
            }

            if (lhs->flags & AST_EXPR_FLAG_CONST && lhs->flags & AST_EXPR_FLAG_CONST) {
                expr->flags |= AST_EXPR_FLAG_CONST;
            }

            break;
        }

        case AST_Expression_Kind::MEMBER: {
            if (!resolve_expression(inst, task, expr->member.base, scope)) {
                return false;
            }

            if (!(expr->member.base->flags & AST_EXPR_FLAG_TYPED)) {

                Type_Task type_task = type_task_create(inst, ast_node(expr->member.base), infer_node(), scope, task->fn_decl, task->bytecode_deps);
                if (!type_expression(inst, &type_task, expr->member.base, scope, nullptr)) {
                    return false;
                }
            }

            Type* base_type = expr->member.base->resolved_type;
            Scope* base_scope = nullptr;
            bool aggregate = false;

            switch (base_type->kind) {
                default: {
                    String tname = temp_type_string(inst, base_type);
                    instance_fatal_error(inst, source_pos(inst, expr->member.base), "Invalid type on left side of '.' operator '%.*s'", (int)tname.length, tname.data);
                    break;
                }

                case Type_Kind::POINTER: {
                    if (base_type->pointer.base->kind != Type_Kind::STRUCT) {
                        String tname = temp_type_string(inst, base_type);
                        instance_fatal_error(inst, source_pos(inst, expr->member.base), "Invalid type on left side of '.' operator '%.*s'", (int)tname.length, tname.data);
                        break;
                    }

                    base_type = base_type->pointer.base;

                    // Falltrough
                }
                case Type_Kind::STRUCT: {
                    base_scope = base_type->structure.scope;
                    aggregate = true;
                    break;
                }

                case Type_Kind::ENUM: {
                    base_scope = base_type->enumeration.scope;
                    break;
                }
            }

            assert(base_scope);
            if (!resolve_identifier(inst, task, expr->member.member_name, base_scope)) {
                return false;
            }

            if (expr->member.base->flags & AST_EXPR_FLAG_CONST) {
                expr->flags |= AST_EXPR_FLAG_CONST;
            } else if (aggregate) {
                expr->flags |= AST_EXPR_FLAG_LVALUE;
            }

            break;
        }

        case AST_Expression_Kind::IMPLICIT_MEMBER: {
            // Identifier is resolved in typer
            break;
        }

        case AST_Expression_Kind::SUBSCRIPT: assert(false); break;

        case AST_Expression_Kind::CALL: {

            if (!resolve_expression(inst, task, expr->call.base, scope)) {
                return false;
            }

            AST_Declaration* callee_decl = nullptr;
            if (expr->call.base->kind == AST_Expression_Kind::IDENTIFIER) {
                callee_decl = expr->call.base->identifier->decl;

                if (!callee_decl) return false;

                assert(task->fn_decl || (expr->flags & AST_EXPR_FLAG_CHILD_OF_RUN));
            } else {
                assert(false);
            }

            assert(callee_decl);

            for (s64 i = 0; i < expr->call.args.count; i++) {

                if (!resolve_expression(inst, task, expr->call.args[i], scope)) {
                    return false;
                }
            }

            break;
        }

        case AST_Expression_Kind::ADDRESS_OF:
        case AST_Expression_Kind::DEREF: {
            if (!resolve_expression(inst, task, expr->unary.operand, scope)) {
                return false;
            }

            break;
        }

        case AST_Expression_Kind::CAST: {
            if (!resolve_ts(inst, task, expr->cast.ts, scope)) {
                return false;
            }

            if (!resolve_expression(inst, task, expr->cast.operand, scope)) {
                return false;
            }

            break;
        }

        case AST_Expression_Kind::COMPOUND: {

            bool all_const = true;

            for (s64 i = 0; i < expr->compound.expressions.count; i++) {

                AST_Expression* cexpr = expr->compound.expressions[i];

                if (!resolve_expression(inst, task, cexpr, scope)) {
                    return false;
                }

                if (!(cexpr->flags & AST_EXPR_FLAG_CONST)) {
                    all_const = false;
                }
            }

            if (all_const) {
                expr->flags |= AST_EXPR_FLAG_CONST;
            }

            break;
        }

        case AST_Expression_Kind::RUN: {

            AST_Expression* run_expr = expr->run.expression;
            assert(run_expr->kind == AST_Expression_Kind::CALL);

            if (!resolve_expression(inst, task, run_expr, scope)) {
                return false;
            }

            for (s64 i = 0; i < run_expr->call.args.count; i++) {

                AST_Expression* arg_expr = run_expr->call.args[i];

                if (!(arg_expr->flags & AST_EXPR_FLAG_CONST)) {
                    instance_fatal_error(inst, source_pos(inst, arg_expr), "Argument expression inside #run must be a constant.");
                }

            }

            break;
        }

        case AST_Expression_Kind::SIZEOF: {

            if (!resolve_expression(inst, task, expr->sizeof_expr.operand, scope)) {
                return false;
            }

            break;
        }

        case AST_Expression_Kind::ALIGNOF: {

            if (!resolve_expression(inst, task, expr->alignof_expr.operand, scope)) {
                return false;
            }

            break;
        }

        case AST_Expression_Kind::OFFSETOF: {
            if (!resolve_identifier(inst, task, expr->offsetof_expr.struct_ident, scope)) {
                return false;
            }

            assert(expr->offsetof_expr.struct_ident->decl);
            AST_Declaration* agg_decl = expr->offsetof_expr.struct_ident->decl;
            assert(agg_decl->kind == AST_Declaration_Kind::STRUCT);
            Scope* agg_scope = agg_decl->structure.scope;

            if (!resolve_identifier(inst, task, expr->offsetof_expr.member_ident, agg_scope)) {
                String member_name = atom_string(expr->offsetof_expr.member_ident->atom);
                String struct_name = atom_string(expr->offsetof_expr.struct_ident->atom);
                instance_fatal_error(inst, source_pos(inst, expr->offsetof_expr.member_ident), "'%s' is not a member of aggregate '%s'",
                        member_name.data, struct_name.data);
                return false;
            }

            AST_Declaration* mem_decl = expr->offsetof_expr.member_ident->decl;
            assert(mem_decl->kind == AST_Declaration_Kind::STRUCT_MEMBER);

            break;
        }

        case AST_Expression_Kind::TYPE: {

            if (!resolve_ts(inst, task, expr->type.type_spec, scope)) {
                return false;
            }

            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL:
        case AST_Expression_Kind::BOOL_LITERAL:
        case AST_Expression_Kind::NULL_LITERAL:
        case AST_Expression_Kind::REAL_LITERAL:
        case AST_Expression_Kind::CHAR_LITERAL:
        case AST_Expression_Kind::STRING_LITERAL: {
            assert(expr->flags & AST_EXPR_FLAG_CONST);
            break;
        }
    }

    expr->flags |= AST_EXPR_FLAG_RESOLVED;
    return true;
}

bool resolve_ts(Instance* inst, Resolve_Task* task, AST_Type_Spec* ts, Scope* scope)
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

        case AST_Type_Spec_Kind::POINTER: {
            result = resolve_ts(inst, task, ts->base, scope);
            break;
        }

        case AST_Type_Spec_Kind::ARRAY: {
            result = true;
            if (!resolve_expression(inst, task, ts->array.length, scope)) {
                result = false;
                break;
            }

            if (!resolve_ts(inst, task, ts->array.element_ts, scope)) {
                result = false;
                break;
            }
            break;
        }
    }

    if (result) {
        ts->flags |= AST_TS_FLAG_RESOLVED;
    }

    return result;
}

bool resolve_identifier(Instance* inst, Resolve_Task* task, AST_Identifier* ident, Scope* scope)
{
    task->waiting_for = nullptr;

    if (ident->decl) return true;

    Scope* found_in_scope;
    if (AST_Declaration* found_decl = scope_find_symbol(scope, ident->atom, &found_in_scope)) {

        if (task->fn_decl &&
            (found_in_scope == task->fn_decl->function.scope ||
             scope_is_parent(found_in_scope, task->fn_decl->function.scope))) {

            // The declaration is inside the current function, check the order...
            if (found_decl->kind == AST_Declaration_Kind::VARIABLE &&
                !(found_decl->flags & AST_DECL_FLAG_PARAM) &&
                found_decl->ident->index >= ident->index) {

                Source_Pos ident_pos = source_pos(inst, ident);
                Source_Pos decl_pos = source_pos(inst, found_decl);

                assert(ident_pos.file_index == decl_pos.file_index);

                const char* ident_string = atom_string(ident->atom).data;
                instance_error(inst, ident_pos, "Reference to identifier '%s' before declaration", ident_string);
                instance_fatal_error_note(inst, decl_pos, "'%s' was first declared here", ident_string);
            }
        }

        ident->decl = found_decl;
        return true;
    }

    task->waiting_for = ident;

    auto name = atom_string(ident->atom);
    log_trace("Waiting for undeclared identifier: '%s'", name.data);
    return false;
}

}
