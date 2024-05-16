#include "typer.h"

#include <containers/darray.h>
#include <memory/allocator.h>
#include <memory/temp_allocator.h>
#include <nstring.h>

#include "ast.h"
#include "instance.h"
#include "resolver.h"
#include "scope.h"
#include "source_pos.h"
#include "task.h"
#include "token.h"
#include "type.h"

#include <assert.h>

namespace Novo {

bool type_node(Instance* inst, Type_Task* task, AST_Node* node, Scope* scope)
{
    switch (node->kind) {

        case AST_Node_Kind::INVALID: assert(false); break;

        case AST_Node_Kind::DECLARATION: {
            return type_declaration(inst, task, node->declaration, scope);
        }

        case AST_Node_Kind::STATEMENT: {
            return type_statement(inst, task, node->statement, scope);
        }

        case AST_Node_Kind::EXPRESSION: {
            Type* inferred_type = infer_type(inst, task, task->infer_type_from, scope);
            return type_expression(inst, task, node->expression, scope, inferred_type);
        }

        case AST_Node_Kind::TYPE_SPEC: {
            return type_type_spec(inst, task, node->ts, scope);
        }
    }

    assert(false);
    return false;
}

bool type_declaration(Instance* inst, Type_Task* task, AST_Declaration* decl, Scope* scope)
{
    if (decl->flags & AST_DECL_FLAG_TYPED) {
        assert(decl->resolved_type);
        return true;
    }

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {

            Type* result_type = nullptr;

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

                if (!result_type) result_type = decl->variable.init_expr->resolved_type;
                else if (valid_implicit_type_conversion(inst, result_type, decl->variable.init_expr->resolved_type)) {
                    Source_Pos pos = source_pos(inst, decl->variable.init_expr);
                    instance_fatal_error(inst, pos, "Mismatching types in variable declaration, expected: '%s', got: '%s'",
                            temp_type_string(inst, result_type).data,
                            temp_type_string(inst, decl->variable.init_expr->resolved_type).data);
                }

                if (!result_type) {
                    result_type = decl->variable.init_expr->resolved_type;
                }
            }

            assert(result_type);

            decl->resolved_type = result_type;
            break;
        }

        case AST_Declaration_Kind::CONSTANT: {

            Type* ts_type = nullptr;

            if (decl->constant.type_spec) {
                if (!type_type_spec(inst, task, decl->constant.type_spec, scope)) {
                    return false;
                }

                ts_type = decl->constant.type_spec->resolved_type;
            }

            if (!type_expression(inst, task, decl->constant.value, scope, ts_type)) {
                return false;
            }

            if (ts_type) assert(ts_type == decl->constant.value->resolved_type);

            decl->resolved_type = decl->constant.value->resolved_type;

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

            Scope* struct_scope = decl->structure.scope;
            auto& fields = decl->structure.members;

            for (s64 i = 0; i < fields.count; i++) {
                auto field = fields[i];

                if (!type_declaration(inst, task, field, struct_scope)) {
                    return false;
                }
            }

            auto struct_members = temp_array_create<Type_Struct_Member>(&inst->temp_allocator, fields.count);

            for (s64 i = 0; i < fields.count; i++) {
                auto field = fields[i];
                assert(field->ident);

                Type_Struct_Member member;
                member.name = field->ident->atom;
                member.type = field->resolved_type;
                darray_append(&struct_members, member);
            }

            decl->resolved_type = struct_type_new(inst, decl->ident->atom, struct_members, struct_scope);

            temp_array_destroy(&struct_members);
            break;
        }

        case AST_Declaration_Kind::ENUM_MEMBER: {

            assert(!decl->enum_member.value_expr);


            Type* inferred_type = infer_type(inst, task, task->infer_type_from, scope);

            if (inferred_type) {
                if (inferred_type->kind != Type_Kind::INTEGER) {
                    String iname = temp_type_string(inst, inferred_type);
                    instance_fatal_error(inst, source_pos(inst, decl), "Enum members must have integer type, got: %.*s", (int)iname.length, iname.data);
                    return false;
                }
                decl->resolved_type = inferred_type;
            } else {
                decl->resolved_type = inst->builtin_type_s64;
            }

            break;
        }

        case AST_Declaration_Kind::ENUM: {

            Scope* enum_scope = decl->enumeration.scope;
            auto& members = decl->enumeration.members;

            Type* strict_type = inst->builtin_type_s64;
            if (decl->enumeration.strict_ts) {
                if (!type_type_spec(inst, task, decl->enumeration.strict_ts, scope)) {
                    return false;
                }

                strict_type = decl->enumeration.strict_ts->resolved_type;
            }
            assert(strict_type);

            for (s64 i = 0; i < members.count; i++) {

                if (!(members[i]->flags & AST_DECL_FLAG_TYPED)) {
                    task->waiting_for = members[i]->ident;
                    return false;
                }
            }

            auto enum_members = temp_array_create<Type_Enum_Member>(&inst->temp_allocator, members.count);

            for (s64 i = 0; i < members.count; i++) {
                AST_Declaration* member = members[i];

                assert(member->resolved_type == strict_type);

                Type_Enum_Member enum_member;
                enum_member.name = member->ident->atom;
                enum_member.value = i;
                darray_append(&enum_members, enum_member);
            }

            decl->resolved_type = enum_type_new(inst, decl->ident->atom, strict_type, enum_members, enum_scope);

            temp_array_destroy(&enum_members);
            break;
        }

        case AST_Declaration_Kind::FUNCTION: {

            if (!decl->resolved_type) {
                for (s64 i = 0; i < decl->function.params.count; i++) {
                    if (!(decl->function.params[i]->flags & AST_DECL_FLAG_TYPED)) {
                        return false;
                    }
                }

                if (decl->function.return_ts && !(decl->function.return_ts->flags & AST_TS_FLAG_TYPED)) {
                    return false;
                }

                auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);
                auto param_types = temp_array_create<Type*>(&inst->temp_allocator, decl->function.params.count);

                for (s64 i = 0; i < decl->function.params.count; i++) {
                    darray_append(&param_types, decl->function.params[i]->resolved_type);
                }

                Type* return_type = decl->function.return_ts ? decl->function.return_ts->resolved_type : inst->builtin_type_void;

                Type_Flags flags = TYPE_FLAG_NONE;
                if (decl->flags & AST_DECL_FLAG_FOREIGN_VARARG) {
                    flags |= TYPE_FLAG_FOREIGN_VARARG;
                }

                decl->resolved_type = function_type_get(inst, param_types, return_type, flags);
                temp_allocator_reset(&inst->temp_allocator_data, mark);
            }

            // Check the body, we might be able to type this function already, but might not be ready to move on to ssa...

            for (s64 i = 0; i < decl->function.body.count; i++) {
                if (!(decl->function.body[i]->flags & AST_STMT_FLAG_TYPED)) {
                    return false;
                }
            }
            break;
        }

        case AST_Declaration_Kind::BUILTIN_TYPE: assert(false); break;
    }

    assert(decl->resolved_type);
    decl->flags |= AST_DECL_FLAG_TYPED;
    return true;
}

bool type_statement(Instance* inst, Type_Task* task, AST_Statement* stmt, Scope* scope)
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

            AST_Expression* lvalue = stmt->assignment.lvalue;
            AST_Expression* rvalue = stmt->assignment.rvalue;

            if (!type_expression(inst, task, lvalue, scope, nullptr)) {
                return false;
            }

            if (!type_expression(inst, task, rvalue, scope, lvalue->resolved_type)) {
                return false;
            }

            if (lvalue->resolved_type != rvalue->resolved_type && !valid_implicit_type_conversion(inst, lvalue->resolved_type, rvalue->resolved_type)) {
                Source_Pos pos = source_pos(inst, stmt);
                instance_fatal_error(inst, pos, "Mismatching types in assignment, left: '%s', right: '%s'",
                        temp_type_string(inst, lvalue->resolved_type).data,
                        temp_type_string(inst, rvalue->resolved_type).data);
            }

            break;
        }

        case AST_Statement_Kind::ARITHMETIC_ASSIGNMENT: {

            AST_Expression* lvalue = stmt->arithmetic_assignment.lvalue;
            AST_Expression* rvalue = stmt->arithmetic_assignment.rvalue;

            if (!type_expression(inst, task, lvalue, scope, nullptr)) {
                return false;
            }

            Type* suggested_type = nullptr;
            bool pointer_math = false;
            if (lvalue->resolved_type->kind == Type_Kind::POINTER) {
                pointer_math = true;
            } else {
                suggested_type = lvalue->resolved_type;
            }

            if (!type_expression(inst, task, rvalue, scope, suggested_type)) {
                return false;
            }

            if (pointer_math) {
                if (!type_pointer_math(inst, task, ast_node(stmt), lvalue, rvalue, stmt->arithmetic_assignment.op, scope)) {
                    return false;
                }

                if (stmt->arithmetic_assignment.op == '-' && rvalue->resolved_type->kind != Type_Kind::INTEGER) {
                    Source_Pos pos = source_pos(inst, rvalue);
                    instance_fatal_error(inst, pos, "Expected integer type on right side of arithmethic assignment (pointer math), got: '%s'",
                            temp_type_string(inst, rvalue->resolved_type).data);
                }
            }

            if (!pointer_math && lvalue->resolved_type != rvalue->resolved_type && !valid_implicit_type_conversion(inst, lvalue->resolved_type, rvalue->resolved_type)) {
                Source_Pos pos = source_pos(inst, stmt);
                instance_fatal_error(inst, pos, "Mismatching types in arithmetic assignment, left: '%s', right: '%s'",
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

            Type* result_type = nullptr;

            assert(task->fn_decl);
            if (task->fn_decl->resolved_type) {
                result_type = task->fn_decl->resolved_type->function.return_type;
            } else if (task->fn_decl->function.return_ts && task->fn_decl->function.return_ts->resolved_type) {
                result_type = task->fn_decl->function.return_ts->resolved_type;
            }

            if (!result_type) {
                return false;
            }

            if (stmt->return_expr) {
                if (!type_expression(inst, task, stmt->return_expr, scope, result_type)) {
                    return false;
                }

                if (result_type != stmt->return_expr->resolved_type && !valid_implicit_type_conversion(inst, result_type, stmt->return_expr->resolved_type)) {
                    Source_Pos pos = source_pos(inst, stmt);
                    instance_fatal_error(inst, pos, "Mismatching type in return statement, got: '%s', expected: '%s'",
                                         temp_type_string(inst, stmt->return_expr->resolved_type).data,
                                         temp_type_string(inst, result_type).data);
                }


            } else {
                assert(result_type->kind == Type_Kind::VOID);
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

            AST_Expression* cond = stmt->while_stmt.cond;
            if (!type_expression(inst, task, cond, scope, inst->builtin_type_bool)) {
                return false;
            }

            if (cond->resolved_type->kind != Type_Kind::BOOLEAN) {
                Source_Pos pos = source_pos(inst, cond);
                instance_fatal_error(inst, pos,  "Expression after 'while' must be of boolean type (got: '%s')",
                                     temp_type_string(inst, cond->resolved_type).data);
            }

            AST_Statement* while_stmt = stmt->while_stmt.stmt;
            if (!type_statement(inst, task, while_stmt, scope)) {
                return false;
            }

            break;
        }

        case AST_Statement_Kind::FOR: {

            AST_Statement* init = stmt->for_stmt.init;
            if (!type_statement(inst, task, init, scope)) {
                return false;
            }

            AST_Expression* cond = stmt->for_stmt.cond;
            if (!type_expression(inst, task, cond, scope, inst->builtin_type_bool)) {
                return false;
            }

            if (cond->resolved_type->kind != Type_Kind::BOOLEAN) {
                Source_Pos pos = source_pos(inst, cond);
                instance_fatal_error(inst, pos, "Conditional in 'for' must be of boolean type (got: '%s')",
                                     temp_type_string(inst, cond->resolved_type).data);
            }

            AST_Statement* step = stmt->for_stmt.step;
            if (!type_statement(inst, task, step, scope)) {
                return false;
            }

            AST_Statement* for_stmt = stmt->for_stmt.stmt;
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

        case AST_Statement_Kind::RUN: {

            AST_Expression* run_expr = stmt->run.expression;
            assert(run_expr->kind == AST_Expression_Kind::CALL);

            DArray<AST_Node> *old_bc_deps = task->bytecode_deps;

            // TODO: Dynamic allocator
            task->bytecode_deps = allocate<DArray<AST_Node>>(c_allocator());
            darray_init(c_allocator(), task->bytecode_deps);

            if (!type_expression(inst, task, run_expr, scope, nullptr)) {
                darray_free(task->bytecode_deps);
                free(c_allocator(), task->bytecode_deps);
                task->bytecode_deps = old_bc_deps;
                return false;
            }

            if (run_expr->resolved_type != inst->type_string && is_pointer_or_parent_of_pointer(run_expr->resolved_type)) {
                instance_fatal_error(inst, source_pos(inst, run_expr), "Type of #run cannot be or contain pointer types, got '%s'", temp_type_string(inst, run_expr->resolved_type).data);
            }

            add_ssa_task(inst, stmt, scope, task->bytecode_deps, nullptr);

            task->bytecode_deps = old_bc_deps;

            if (task->bytecode_deps) {
                darray_append(task->bytecode_deps, ast_node(stmt));
            }

            break;
        }

        case AST_Statement_Kind::INSERT: {

            AST_Expression* insert_expr = stmt->insert.expression;
            assert(insert_expr->kind == AST_Expression_Kind::CALL);

            DArray<AST_Node> *old_bc_deps = task->bytecode_deps;

            task->bytecode_deps = allocate<DArray<AST_Node>>(c_allocator());
            darray_init(c_allocator(), task->bytecode_deps);

            if (!type_expression(inst, task, insert_expr, scope, inst->type_string)) {
                darray_free(task->bytecode_deps);
                free(c_allocator(), task->bytecode_deps);
                task->bytecode_deps = old_bc_deps;
                return false;
            }

            if (insert_expr->resolved_type != inst->type_string) {
                instance_fatal_error(inst, source_pos(inst, insert_expr), "Type of #insert must be string, got '%s'", temp_type_string(inst, insert_expr->resolved_type).data);
            }

            add_ssa_task(inst, stmt, scope, task->bytecode_deps, old_bc_deps);

            task->bytecode_deps = old_bc_deps;

            if (task->bytecode_deps) {
                darray_append(task->bytecode_deps, ast_node(stmt));
            }

            break;
        }

        case AST_Statement_Kind::BLOCK: {

            for (s64 i = 0; i < stmt->block.statements.count; i++) {
                if (!type_statement(inst, task, stmt->block.statements[i], stmt->block.scope)) {
                    return false;
                }
            }
            break;
        }

        case AST_Statement_Kind::ASSERT: {

            AST_Expression* cond = stmt->assert_stmt.cond;
            AST_Expression* msg = stmt->assert_stmt.message;

            if (!type_expression(inst, task, cond, scope, inst->builtin_type_bool)) {
                return false;
            }

            if (msg && !type_expression(inst, task, msg, scope, nullptr)) {
                return false;
            }

            if (cond->resolved_type->kind != Type_Kind::BOOLEAN) {
                Source_Pos pos = source_pos(inst, cond);
                instance_fatal_error(inst, pos, "Assert condition must be of boolean type");
                assert(false);
            }

            if (msg && msg->resolved_type != inst->type_string) {
                Source_Pos pos = source_pos(inst, msg);
                instance_fatal_error(inst, pos, "Assert message must be of string type");
                assert(false);
            }

            break;
        }
    }

    stmt->flags |= AST_TS_FLAG_TYPED;
    return true;
}

bool type_expression(Instance* inst, Type_Task* task, AST_Expression* expr, Scope* scope, Type* suggested_type)
{
    if (expr->flags & AST_EXPR_FLAG_TYPED) {
        assert(expr->resolved_type);
        return true;
    }

    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            auto decl = expr->identifier->decl;

            if (!type_identifier(inst, task, expr->identifier, scope, suggested_type)) {
                return false;
            }

            expr->resolved_type = decl->resolved_type;

            if (decl->kind == AST_Declaration_Kind::CONSTANT &&
                suggested_type && suggested_type != decl->resolved_type) {

                assert(decl->resolved_type->kind == Type_Kind::INTEGER);

                expr->resolved_type = suggested_type;
            }
            break;
        }

        case AST_Expression_Kind::UNARY: {

            if (!type_expression(inst, task, expr->unary.operand, scope, suggested_type)) {
                return false;
            }


            Type* op_type = expr->unary.operand->resolved_type;
            assert(suggested_type ? suggested_type == op_type : true);

            if (op_type->kind != Type_Kind::INTEGER) {
                Source_Pos pos = source_pos(inst, expr->unary.operand);
                instance_fatal_error(inst, pos, "Operand to unary '-' must have signed integer type, got: '%s'", temp_type_string(inst, op_type).data);
                assert(false);
            }

            if (!op_type->integer.sign) {
                Source_Pos pos = source_pos(inst, expr->unary.operand);
                instance_fatal_error(inst, pos, "Operand to unary '-'' must have signed integer type, got: '%s'", temp_type_string(inst, op_type).data);
                assert(false);
            }

            expr->resolved_type = op_type;
            break;
        }

        case AST_Expression_Kind::BINARY: {
            AST_Expression* lhs = expr->binary.lhs;
            AST_Expression* rhs = expr->binary.rhs;

            if (suggested_type && suggested_type->kind == Type_Kind::BOOLEAN) {
                suggested_type = nullptr;
            }

            if (!type_expression(inst, task, lhs, scope, suggested_type)) {
                return false;
            }

            if (!suggested_type) {
                suggested_type = lhs->resolved_type;
            }

            if (suggested_type->kind == Type_Kind::POINTER) {
                suggested_type = nullptr;
            }

            if (!type_expression(inst, task, rhs, scope, suggested_type)) {
                return false;
            }

            Type* left_type = lhs->resolved_type;

            if (left_type->kind == Type_Kind::POINTER && !is_binary_cmp_op(expr->binary.op)) {

                Type* result_type = type_pointer_math(inst, task, ast_node(expr), lhs, rhs, expr->binary.op, scope);

                if (!result_type) {
                    return false;
                }
                expr->resolved_type = result_type;

            } else {

                if (lhs->resolved_type->kind != Type_Kind::INTEGER &&
                    lhs->resolved_type->kind != Type_Kind::POINTER) {

                    instance_fatal_error(inst, source_pos(inst, lhs), "Expected integer or pointer type on left side of binary operator '%s', got: '%s'",
                            tmp_token_kind_str((Token_Kind)expr->binary.op).data,
                            temp_type_string(inst, lhs->resolved_type).data);
                }

                if (lhs->resolved_type != rhs->resolved_type && !valid_implicit_type_conversion(inst, lhs->resolved_type, rhs->resolved_type)) {
                    instance_fatal_error(inst, source_pos(inst, expr), "Mismatching types in binary expression: '%s' %s '%s'",
                            temp_type_string(inst, lhs->resolved_type).data,
                            tmp_token_kind_str((Token_Kind)expr->binary.op).data,
                            temp_type_string(inst, rhs->resolved_type).data);
                }

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
                        expr->resolved_type = lhs->resolved_type;
                        break;
                    }
                }

            }

            assert(expr->resolved_type);
            break;
        }

        case AST_Expression_Kind::MEMBER: {

            if (!type_expression(inst, task, expr->member.base, scope, nullptr)) {
                return false;
            }

            Type* base_type = expr->member.base->resolved_type;
            Type *struct_type = nullptr;

            if (base_type->kind == Type_Kind::STRUCT) {
                struct_type = base_type;
            } else {
                assert(base_type->kind == Type_Kind::POINTER);
                assert(base_type->pointer.base->kind == Type_Kind::STRUCT);

                struct_type = base_type->pointer.base;
            }

            assert(struct_type);
            Scope *struct_scope = struct_type->structure.scope;

            AST_Declaration* field = scope_find_symbol(struct_scope, expr->member.member_name->atom, nullptr);
            u32 index = field->variable.index;
            assert(index >= 0 && index < struct_type->structure.members.count);

            Type* mem_type = struct_type->structure.members[index].type;

            expr->resolved_type = mem_type;
            break;
        }

        case AST_Expression_Kind::CALL: {

            AST_Expression* base = expr->call.base;

            if (!type_expression(inst, task, base, scope, nullptr)) {
                return false;
            }

            assert(base->resolved_type->kind == Type_Kind::FUNCTION);
            auto fn_type = base->resolved_type;

            if (expr->call.args.count != fn_type->function.param_types.count) {

                bool foreign_vararg = fn_type->flags & TYPE_FLAG_FOREIGN_VARARG;

                bool valid_foreign_varargs = foreign_vararg &&
                                             expr->call.args.count > fn_type->function.param_types.count;


                if (!valid_foreign_varargs) {
                    Source_Pos pos = source_pos(inst, expr);
                    instance_fatal_error(inst, pos, "Invalid argument count");
                }
            }

            for (s64 i = 0; i < expr->call.args.count; i++) {

                Type* param_type = nullptr;

                if (i < fn_type->function.param_types.count) {
                    param_type = fn_type->function.param_types[i];
                }

                if (!type_expression(inst, task, expr->call.args[i], scope, param_type)) {
                    return false;
                }

                AST_Expression* arg_expr = expr->call.args[i];

                if (param_type && arg_expr->resolved_type != param_type && !valid_implicit_type_conversion(inst, arg_expr->resolved_type, param_type)) {

                    Source_Pos pos = source_pos(inst, arg_expr);

                    instance_fatal_error(inst, pos, "Mismatching type for argument %d, got: '%s', expected: '%s'", i + 1,
                            temp_type_string(inst, arg_expr->resolved_type).data,
                            temp_type_string(inst, param_type).data);
                    assert(false);
                }
            }

            bool child_of_run = expr->flags & AST_EXPR_FLAG_CHILD_OF_RUN;

            expr->resolved_type = fn_type->function.return_type;
            assert(task->fn_decl || child_of_run);
            for (s64 i = 0; i < expr->call.args.count; i++) {
                if (expr->call.args[i]->resolved_type->kind == Type_Kind::STRUCT && !child_of_run) {
                    darray_append_unique(&task->fn_decl->function.temp_structs, expr->call.args[i]);
                }
            }

            if (expr->resolved_type->kind == Type_Kind::STRUCT && !child_of_run) {
                assert(fn_type->function.return_type->kind == Type_Kind::STRUCT);
                darray_append_unique(&task->fn_decl->function.temp_structs, expr);
            }

            assert(base->kind == AST_Expression_Kind::IDENTIFIER);
            AST_Declaration* callee_decl = base->identifier->decl;

            assert(callee_decl);
            if (callee_decl != task->fn_decl) {
                assert(task->bytecode_deps);
                darray_append_unique(task->bytecode_deps, ast_node(callee_decl));
            }
            break;
        }

        case AST_Expression_Kind::ADDRESS_OF: {

            if (!type_expression(inst, task, expr->unary.operand, scope, nullptr)) {
                return false;
            }

            if (!(expr->unary.operand->flags & AST_EXPR_FLAG_LVALUE)) {
                Source_Pos pos = source_pos(inst, expr->unary.operand);
                instance_fatal_error(inst, pos, "Cannot take address of non lvalue expression");
                assert(false);
                return false;
            }

            assert(expr->unary.operand->flags & AST_EXPR_FLAG_LVALUE);

            expr->resolved_type = pointer_type_get(inst, expr->unary.operand->resolved_type);
            break;
        }

        case AST_Expression_Kind::DEREF: {

            if (!type_expression(inst, task, expr->unary.operand, scope, nullptr)) {
                return false;
            }

            if (expr->unary.operand->resolved_type->kind != Type_Kind::POINTER) {
                Source_Pos pos = source_pos(inst, expr->unary.operand);
                instance_fatal_error(inst, pos, "Cannot dereference non pointer expression");
                assert(false);
                return false;
            }

            expr->resolved_type = expr->unary.operand->resolved_type->pointer.base;
            break;
        }

        case AST_Expression_Kind::CAST: {

            if (!type_type_spec(inst, task, expr->cast.ts, scope)) {
                return false;
            }

            if (!type_expression(inst, task, expr->cast.operand, scope, nullptr)) {
                return false;
            }

            Type* from_type = expr->cast.operand->resolved_type;
            Type* to_type = expr->cast.ts->resolved_type;

            if (!valid_cast(inst, from_type, to_type, expr)) {
                return false;
            }

            expr->resolved_type = expr->cast.ts->resolved_type;
            break;
        }

        case AST_Expression_Kind::COMPOUND: {
            assert(suggested_type);
            assert(suggested_type->kind == Type_Kind::STRUCT);

            assert(suggested_type->structure.members.count == expr->compound.expressions.count);

            for (s64 i = 0; i < expr->compound.expressions.count; i++) {
                if (!type_expression(inst, task, expr->compound.expressions[i], scope, suggested_type->structure.members[i].type)) {
                    return false;
                }
            }

            if (!(expr->flags & AST_EXPR_FLAG_CONST)) {
                darray_append_unique(&task->fn_decl->function.temp_structs, expr);
            }
            expr->resolved_type = suggested_type;
            break;
        }

        case AST_Expression_Kind::RUN: {

            AST_Expression* run_expr = expr->run.expression;
            assert(run_expr->kind == AST_Expression_Kind::CALL);

            DArray<AST_Node> *old_bc_deps = task->bytecode_deps;

            task->bytecode_deps = allocate<DArray<AST_Node>>(c_allocator());
            darray_init(c_allocator(), task->bytecode_deps);

            if (!type_expression(inst, task, run_expr, scope, suggested_type)) {
                darray_free(task->bytecode_deps);
                free(c_allocator(), task->bytecode_deps);
                task->bytecode_deps = old_bc_deps;
                return false;
            }

            if (run_expr->resolved_type->kind == Type_Kind::VOID) {
                instance_fatal_error(inst, source_pos(inst, run_expr), "#run at expression level must return a value, got 'void'");
            } else if (run_expr->resolved_type != inst->type_string && is_pointer_or_parent_of_pointer(run_expr->resolved_type)) {
                instance_fatal_error(inst, source_pos(inst, run_expr), "Type of #run cannot be or contain pointer types, got '%s'", temp_type_string(inst, run_expr->resolved_type).data);
            }

            expr->resolved_type = run_expr->resolved_type;

            add_ssa_task(inst, expr, scope, task->bytecode_deps, nullptr);

            task->bytecode_deps = old_bc_deps;

            darray_append(task->bytecode_deps, ast_node(expr));

            break;
        }

        case AST_Expression_Kind::SIZEOF: {
            if (!type_expression(inst, task, expr->sizeof_expr.operand, scope, nullptr)) {
                return false;
            }

            expr->resolved_type = inst->builtin_type_int;
            break;
        }

        case AST_Expression_Kind::ALIGNOF: {
            if (!type_expression(inst, task, expr->alignof_expr.operand, scope, nullptr)) {
                return false;
            }

            expr->resolved_type = inst->builtin_type_int;
            break;
        }

        case AST_Expression_Kind::OFFSETOF: {
            if (!type_identifier(inst, task, expr->offsetof_expr.struct_ident, scope, suggested_type)) {
                return false;
            }

            AST_Declaration* agg_decl = expr->offsetof_expr.struct_ident->decl;
            assert(agg_decl->kind == AST_Declaration_Kind::STRUCT);
            Scope* agg_scope = agg_decl->structure.scope;

            if (!type_identifier(inst, task, expr->offsetof_expr.member_ident, agg_scope, suggested_type)) {
                return false;
            }

            AST_Declaration* mem_decl = expr->offsetof_expr.member_ident->decl;
            assert(mem_decl->kind == AST_Declaration_Kind::STRUCT_MEMBER);

            expr->resolved_type = inst->builtin_type_int;
            break;
        }

        case AST_Expression_Kind::TYPE: {

            if (!type_type_spec(inst, task, expr->type.type_spec, scope)) {
                return false;
            }

            expr->resolved_type = expr->type.type_spec->resolved_type;
            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL: {

            if (suggested_type) {
                assert(suggested_type->kind == Type_Kind::INTEGER);

                bool hex = expr->flags & AST_EXPR_FLAG_HEX_LITERAL;
                bool binary = expr->flags & AST_EXPR_FLAG_BINARY_LITERAL;

                bool check_signed = !hex && !binary;

                bool fit = false;

                if (suggested_type->integer.sign && check_signed) {
                    s64 val = (s64)expr->integer_literal;
                    switch (suggested_type->bit_size) {
                        default: assert(false); break;
                        case 8:  fit = (val >= I8_MIN && val <= I8_MAX); break;
                        case 16: fit = (val >= I16_MIN && val <= I16_MAX); break;
                        case 32: fit = (val >= I32_MIN && val <= I32_MAX); break;
                        case 64: fit = (val >= I64_MIN && val <= I64_MAX); break;
                    }
                } else {
                    u64 val = (u64)expr->integer_literal;
                    switch (suggested_type->bit_size) {
                        default: assert(false); break;
                        case 8:  fit = (val <= U8_MAX); break;
                        case 16: fit = (val <= U16_MAX); break;
                        case 32: fit = (val <= U32_MAX); break;
                        case 64: fit = (val <= U64_MAX); break;
                    }
                }

                if (!fit) {
                    Source_Pos pos = source_pos(inst, expr);
                    instance_fatal_error(inst, pos, "Integer literal %d (0x%x) not within bounds of suggested type '%s'",
                            expr->integer_literal, expr->integer_literal,
                            temp_type_string(inst, suggested_type).data);
                }

                expr->resolved_type = suggested_type;

            } else {
                expr->resolved_type = inst->builtin_type_s64;
            }

            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;

        case AST_Expression_Kind::CHAR_LITERAL: {
            if (suggested_type) {
                assert(suggested_type->bit_size  >= inst->builtin_type_u8->bit_size);
                expr->resolved_type = suggested_type;
            } else {
                expr->resolved_type = inst->builtin_type_u8;
            }
            break;
        }

        case AST_Expression_Kind::BOOL_LITERAL: {
            if (suggested_type) {
                assert(suggested_type->kind == Type_Kind::BOOLEAN);
                expr->resolved_type = suggested_type;
            } else {
                expr->resolved_type = inst->builtin_type_bool;
            }
            break;
        }

        case AST_Expression_Kind::NULL_LITERAL: {
            assert(suggested_type);
            assert(suggested_type->kind == Type_Kind::POINTER);

            expr->resolved_type = suggested_type;
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: {

            assert(inst->type_string);
            expr->resolved_type = inst->type_string;
            break;
        }
    }

    assert(expr->resolved_type);
    expr->flags |= AST_EXPR_FLAG_TYPED;
    return true;
}

bool type_type_spec(Instance* inst, Type_Task* task, AST_Type_Spec* ts, Scope* scope)
{
    if (ts->flags & AST_TS_FLAG_TYPED) {
        assert(ts->resolved_type);
        return true;
    }

    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false); break;

        case AST_Type_Spec_Kind::IDENTIFIER: {
            if (!type_identifier(inst, task, ts->identifier, scope, nullptr)) {
                return false;
            }

            ts->resolved_type = ts->identifier->decl->resolved_type;
            break;
        }

        case AST_Type_Spec_Kind::POINTER: {

            if (!type_type_spec(inst, task, ts->base, scope)) {
                return false;
            }

            ts->resolved_type = pointer_type_get(inst, ts->base->resolved_type);
            break;
        }
    }

    assert(ts->resolved_type);
    ts->flags |= AST_TS_FLAG_TYPED;
    return true;
}

bool type_identifier(Instance* inst, Type_Task*task, AST_Identifier* ident, Scope* scope, Type* suggested_type)
{
    task->waiting_for = nullptr;

    if (ident->decl->flags & AST_DECL_FLAG_TYPED) {
        assert(ident->decl->resolved_type);
        return true;
    }

    if (ident->decl->kind == AST_Declaration_Kind::FUNCTION &&
        ident->decl->resolved_type) {
        return true;
    }

    task->waiting_for = ident;
    return false;
}

Type* type_pointer_math(Instance* inst, Type_Task* task, AST_Node err_node, AST_Expression* left, AST_Expression* right, u32 op, Scope* scope)
{
    Type* left_type = left->resolved_type;
    Type* right_type = right->resolved_type;

    assert(left_type->kind == Type_Kind::POINTER);

    switch (right_type->kind) {

        case Type_Kind::POINTER: {
            if (op != '-') {
                Source_Pos pos = source_pos(inst, err_node);
                instance_fatal_error(inst, pos, "Invalid operator in pointer math binary expression. Only '-' is allowed when both sides are of pointer type");
            }

            return inst->builtin_type_s64;
        }

        case Type_Kind::INTEGER: {
            if (op != '-' && op != '+') {
                Source_Pos pos = source_pos(inst, err_node);
                instance_fatal_error(inst, pos, "Invalid operator in pointer math binary expression. Only '+' and '-' are allowed");
            }

            return left_type;
        }

        default: {
            Source_Pos pos = source_pos(inst, err_node);
            instance_fatal_error(inst, pos, "Invalid type in right side of pointer math binary expression. Expected integer or pointer, got '%s'",
                    temp_type_string(inst, right_type));
            break;
        }
    }

    return left_type;
}

bool valid_cast(Instance* inst, Type* from_type, Type* to_type, AST_Expression* err_node)
{
    Type_Kind to_kind = to_type->kind;

    bool result = false;
    bool error_reported = false;

    switch (from_type->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;

        case Type_Kind::INTEGER: {

            switch (to_kind) {
                case Type_Kind::INTEGER: result = true; break;

                case Type_Kind::POINTER: {
                    result = to_type->bit_size == from_type->bit_size;

                    if (!result) {
                        error_reported = true;
                        Source_Pos pos = source_pos(inst, err_node);
                        instance_error(inst, pos, "Illegal cast from pointer to integer: '%s', to: '%s'",
                                temp_type_string(inst, from_type).data,
                                temp_type_string(inst, to_type).data);
                        instance_fatal_error_note(inst, pos, "Integer size must match pointer size");
                    }
                    break;
                }

                default: result = false; break;
            }
            break;
        }

        case Type_Kind::BOOLEAN: assert(false); break;

        case Type_Kind::POINTER: {

            switch (to_kind) {
                case Type_Kind::POINTER: {
                    return true;
                    break;
                }

                case Type_Kind::INTEGER: {
                    result = to_type->bit_size == from_type->bit_size;

                    if (!result) {
                        error_reported = true;
                        Source_Pos pos = source_pos(inst, err_node);
                        instance_error(inst, pos, "Illegal cast from pointer to integer: '%s', to: '%s'",
                                temp_type_string(inst, from_type).data,
                                temp_type_string(inst, to_type).data);
                        instance_fatal_error_note(inst, pos, "Integer size must match pointer size");
                    }
                    break;
                }

                default: result = false; break;
            }

            break;
        }

        case Type_Kind::FUNCTION: assert(false); break;
        case Type_Kind::STRUCT: assert(false); break;
        case Type_Kind::ENUM: assert(false); break;
    }

    if (!result && !error_reported) {
        Source_Pos pos = source_pos(inst, err_node);
        instance_fatal_error(inst, pos, "Illegal type conversion in cast, from: '%s', to: '%s'",
                temp_type_string(inst, from_type).data,
                temp_type_string(inst, to_type).data);
    }

    assert(!error_reported);

    return result;
}

}
