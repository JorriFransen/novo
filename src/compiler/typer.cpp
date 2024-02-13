#include "typer.h"

#include <containers/darray.h>
#include <memory/temp_allocator.h>
#include <nstring.h>

#include "ast.h"
#include "instance.h"
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

        case AST_Node_Kind::EXPRESSION: assert(false); break;

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

            Scope* struct_scope = decl->structure.scope;
            auto &fields = decl->structure.fields;

            for (s64 i = 0; i < fields.count; i++) {
                auto field = fields[i];

                if (!type_declaration(inst, task, field, struct_scope)) {
                    return false;
                }
            }

            auto member_types = temp_array_create<Type*>(&inst->temp_allocator, fields.count);

            for (s64 i = 0; i < fields.count; i++) {
                auto field = fields[i];
                darray_append(&member_types, field->resolved_type);
            }

            decl->resolved_type = struct_type_new(inst, decl->ident->atom, member_types, struct_scope);

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

            if (lvalue->resolved_type != rvalue->resolved_type) {
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
            }

            if (!pointer_math && lvalue->resolved_type != rvalue->resolved_type) {
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
            if (stmt->return_expr) {

                Type* result_type = nullptr;

                assert(task->fn_decl);
                if (task->fn_decl->resolved_type) {
                    result_type = task->fn_decl->resolved_type->function.return_type;
                } else if (task->fn_decl->function.return_ts->resolved_type) {
                    result_type = task->fn_decl->function.return_ts->resolved_type;
                }

                if (!result_type) {
                    return false;
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
            if (!decl) {
                return false;
            }

            if (!(decl->flags & AST_DECL_FLAG_TYPED)) {
                return false;
            }


            if (decl->kind == AST_Declaration_Kind::VARIABLE) {
                expr->flags |= AST_EXPR_FLAG_LVALUE;
            }

            expr->resolved_type = decl->resolved_type;
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
            if (suggested_type && suggested_type->kind == Type_Kind::BOOLEAN) {
                suggested_type = nullptr;
            }

            if (!type_expression(inst, task, expr->binary.lhs, scope, suggested_type)) {
                return false;
            }

            if (!suggested_type) {
                suggested_type = expr->binary.lhs->resolved_type;
            }

            if (suggested_type->kind == Type_Kind::POINTER) {
                suggested_type = nullptr;
            }

            if (!type_expression(inst, task, expr->binary.rhs, scope, suggested_type)) {
                return false;
            }

            Type* left_type = expr->binary.lhs->resolved_type;

            if (left_type->kind == Type_Kind::POINTER && !is_binary_cmp_op(expr->binary.op)) {

                Type* result_type = type_pointer_math(inst, task, ast_node(expr), expr->binary.lhs, expr->binary.rhs, expr->binary.op, scope);

                if (!result_type) {
                    return false;
                }
                expr->resolved_type = result_type;

            } else {

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

            expr->flags |= AST_EXPR_FLAG_LVALUE;
            expr->resolved_type = mem_type;
            break;
        }

        case AST_Expression_Kind::CALL: {

            if (!type_expression(inst, task, expr->call.base, scope, nullptr)) {
                return false;
            }

            assert(expr->call.base->resolved_type->kind == Type_Kind::FUNCTION);
            auto fn_type = expr->call.base->resolved_type;

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

                if (param_type && arg_expr->resolved_type != param_type) {

                    Source_Pos pos = source_pos(inst, arg_expr);

                    instance_fatal_error(inst, pos, "Mismatching type for argument %d, got: '%s', expected: '%s'", i + 1,
                            temp_type_string(inst, arg_expr->resolved_type).data,
                            temp_type_string(inst, param_type).data);
                    assert(false);
                }
            }

            expr->resolved_type = fn_type->function.return_type;
            assert(task->fn_decl);
            for (s64 i = 0; i < expr->call.args.count; i++) {
                if (expr->call.args[i]->resolved_type->kind == Type_Kind::STRUCT) {
                    darray_append_unique(&task->fn_decl->function.temp_structs, expr->call.args[i]);
                }
            }

            if (expr->resolved_type->kind == Type_Kind::STRUCT) {
                assert(fn_type->function.return_type->kind == Type_Kind::STRUCT);
                darray_append_unique(&task->fn_decl->function.temp_structs, expr);
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
            auto decl = ts->identifier->decl;
            assert(decl);

            if (!decl->resolved_type) {
                return false;
            }

            ts->resolved_type = decl->resolved_type;
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

            break;
        }

        case Type_Kind::INTEGER: {
            if (op != '-' && op != '+') {
                Source_Pos pos = source_pos(inst, err_node);
                instance_fatal_error(inst, pos, "Invalid operator in pointer math binary expression. Only '+' and '-' are allowed");
            }

            break;
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
