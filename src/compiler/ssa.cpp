#include "ssa.h"

#include <containers/stack.h>
#include <memory/temp_allocator.h>
#include <string_builder.h>

#include "ast.h"
#include "instance.h"
#include "lexer.h"
#include "source_pos.h"
#include "type.h"

#include <assert.h>

namespace Novo {

struct SSA_Alloc
{
    AST_Node ast_node;
    u32 alloc_reg;
};

struct SSA_Break_Info
{
    u32 break_block;
    u32 continue_block;
};

struct SSA_Builder
{
    SSA_Program *program;
    SSA_Function *function;
    s64 block_index;

    Stack<SSA_Break_Info> break_info_stack;
};


void ssa_program_init(SSA_Program *program, Allocator *allocator)
{
    program->allocator = allocator;
    program->entry_fn_index = -1;
    darray_init(allocator, &program->functions);
}

void ssa_program_free(SSA_Program *program)
{
    for (s64 fi = 0; fi < program->functions.count; fi++) {
        SSA_Function *func = &program->functions[fi];

        for (s64 bi = 0; bi < func->blocks.count; bi++) {
            SSA_Block *block = &func->blocks[bi];

            darray_free(&block->bytes);
            darray_free(&block->incoming);
        }

        darray_free(&func->blocks);
        darray_free(&func->allocs);

    }

    darray_free(&program->functions);
}

void ssa_function_init(SSA_Program *program, SSA_Function *func, Atom name, u32 param_count, bool sret)
{
    func->name = name;
    func->register_count = 0;
    func->param_count = param_count;
    darray_init(program->allocator, &func->blocks);
    darray_init(program->allocator, &func->allocs);

    ssa_block_create(program, func, "entry");

    func->sret = sret;
    if (sret) func->param_count++;
}

void ssa_block_init(SSA_Program *program, SSA_Function *func, SSA_Block *block, Atom name)
{
    block->base_name = name;

    int count = 0;
    for (s64 i = 0; i < func->blocks.count; i++) {
        if (func->blocks[i].base_name == name) {
            count++;
        }
    }

    if (count) {
        const int name_buf_size = 128;
        char name_buf[name_buf_size];
        s32 new_length = string_format(name_buf, "%s.%d", atom_string(name).data, count);

        name = atom_get(name_buf, new_length);
    }

    block->name = name;
    darray_init(program->allocator, &block->bytes);
    block->exits = false;
    darray_init(program->allocator, &block->incoming, 0);
    block->next_index = -1;
}

void ssa_block_init(SSA_Program *program, SSA_Function *func, SSA_Block *block, const char *name)
{
    return ssa_block_init(program, func, block, atom_get(name));
}

u32 ssa_block_create(SSA_Program *program, SSA_Function *function, const char *name)
{
    SSA_Block result;
    ssa_block_init(program, function, &result, name);
    s64 index = function->blocks.count;
    assert(index >= 0 && U32_MAX);

    darray_append(&function->blocks, result);
    return (u32)index;
}

u32 ssa_block_create(SSA_Builder *builder, const char *name)
{
    return ssa_block_create(builder->program, builder->function, name);
}

u32 ssa_register_create(SSA_Builder *builder)
{
    assert(builder->function->register_count != U32_MAX);
    return builder->function->register_count++;
}

bool ssa_emit_function(Instance *inst, SSA_Program *program, AST_Declaration *decl)
{
    SSA_Function func;
    assert(decl->ident);

    bool sret = decl->resolved_type->function.return_type->kind == Type_Kind::STRUCT;

    ssa_function_init(program, &func, decl->ident->atom, decl->function.params.count, sret);

    SSA_Builder local_builder;
    local_builder.program = program;
    local_builder.function = &func;
    local_builder.block_index = 0;

    auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);
    stack_init(&inst->temp_allocator, &local_builder.break_info_stack, 0);

    SSA_Builder *builder = &local_builder;

    auto scope = decl->function.scope;

    // Emit storage for parameters
    for (s64 i = 0; i < decl->function.params.count; i++) {

        auto param_decl = decl->function.params[i];

        if (param_decl->flags & AST_DECL_FLAG_STORAGE_REQUIRED) {

            u32 alloc_reg = ssa_emit_alloc(builder, param_decl->resolved_type->bit_size);
            darray_append(&func.allocs, { ast_node(param_decl), alloc_reg });
        }
    }

    // Emit storage for local variables
    for (s64 i = 0; i < decl->function.variables.count; i++) {

        auto var_decl = decl->function.variables[i];

        u32 alloc_reg = ssa_emit_alloc(builder, var_decl->resolved_type->bit_size);
        darray_append(&func.allocs, { ast_node(var_decl), alloc_reg });
    }

    // Emit storage for (temporary) aggregates
    for (s64 i = 0; i < decl->function.temp_structs.count; i++) {

        auto expr = decl->function.temp_structs[i];

        u32 alloc_reg = ssa_emit_alloc(builder, expr->resolved_type->bit_size);
        darray_append(&func.allocs, { ast_node(expr), alloc_reg });
    }

    // Copy parameters into local storage
    u32 param_storage_index = 0;
    for (s64 i = 0; i < decl->function.params.count; i++) {
        AST_Declaration *param_decl = decl->function.params[i];

        if (param_decl->flags & AST_DECL_FLAG_STORAGE_REQUIRED) {

            u32 param_index = i;
            if (sret) param_index++;
            u32 param_reg = ssa_emit_load_param(builder, param_index);

            ssa_emit_store_ptr(builder, param_decl->resolved_type->bit_size, param_storage_index++, param_reg);
        }
    }

    // Emit the body
    for (s64 i = 0; i < decl->function.body.count; i++) {
        auto stmt = decl->function.body[i];

        if (ssa_block_exits(builder, builder->block_index)) {
            auto pos = source_range_start(inst, stmt->range_id);
            instance_fatal_error(inst, pos, "Unreachable code detected");
        }
        ssa_emit_statement(builder, stmt, scope);
    }

    if (!ssa_block_exits(builder, builder->block_index) && func.blocks[builder->block_index].incoming.count > 0) {
        u32 sp_id;
        if (decl->function.body.count) {
            sp_id = source_range_end(inst, decl->function.body[decl->function.body.count - 1]->range_id);
        } else {
            sp_id = source_range_end(inst, decl->range_id);
        }
        instance_fatal_error(inst, sp_id, "Function '%s' does not return a value from all control paths", atom_string(func.name).data);
    }

    if (decl->ident->atom == atom_get("main")) {
        assert(program->entry_fn_index == -1);
        program->entry_fn_index = program->functions.count;
    }
    darray_append(&program->functions, func);

    temp_allocator_reset(&inst->temp_allocator_data, mark);

    return true;
}

bool ssa_find_function(SSA_Program *program, Atom atom, u32 *index)
{
    bool found = false;
    for (s64 i = 0; i < program->functions.count; i++) {
        if (program->functions[i].name == atom) {
            found = true;
            if (index) *index = i;
            break;
        }
    }
    return found;
}

bool ssa_find_alloc(SSA_Builder *builder, AST_Node *ast_node, u32 *result)
{
    for (s64 i = 0; i < builder->function->allocs.count; i++) {

        if (builder->function->allocs[i].ast_node == *ast_node) {
            *result = builder->function->allocs[i].alloc_reg;
            return true;
        }
    }

    return false;
}

bool ssa_find_alloc(SSA_Builder *builder, AST_Declaration *decl, u32 *result)
{
    AST_Node node = ast_node(decl);
    return ssa_find_alloc(builder, &node, result);
}

bool ssa_find_alloc(SSA_Builder *builder, AST_Expression *expr, u32 *result)
{
    AST_Node node = ast_node(expr);
    return ssa_find_alloc(builder, &node, result);
}

void ssa_set_insert_point(SSA_Builder *builder, u32 new_block_index)
{
    assert(new_block_index >= 0 && new_block_index < builder->function->blocks.count);

    if (builder->block_index == new_block_index) return;

    builder->function->blocks[builder->block_index].next_index = new_block_index;
    builder->block_index = new_block_index;
}

bool ssa_block_exits(SSA_Builder *builder, s64 block_index)
{
    assert(block_index >= 0 && block_index < builder->function->blocks.count);

    return builder->function->blocks[block_index].exits;
}

void ssa_emit_statement(SSA_Builder *builder, AST_Statement *stmt, Scope *scope)
{
    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;
        case AST_Statement_Kind::IMPORT: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            if (stmt->declaration->kind == AST_Declaration_Kind::VARIABLE) {

                AST_Expression *init_expr = stmt->declaration->variable.init_expr;
                if (init_expr) {

                    u32 alloc_reg;
                    bool found = ssa_find_alloc(builder, stmt->declaration, &alloc_reg);
                    assert(found);

                    switch (init_expr->resolved_type->kind) {

                        case Type_Kind::INVALID: assert(false); break;
                        case Type_Kind::VOID: assert(false); break;

                        case Type_Kind::INTEGER:
                        case Type_Kind::BOOLEAN: {
                            u32 value_reg = ssa_emit_expression(builder, init_expr, scope);
                            ssa_emit_store_ptr(builder, init_expr->resolved_type->bit_size, alloc_reg, value_reg);
                            break;
                        }

                        case Type_Kind::FUNCTION: assert(false); break;

                        case Type_Kind::STRUCT: {
                            u32 value_reg = ssa_emit_lvalue(builder, init_expr, scope);
                            ssa_emit_memcpy(builder, alloc_reg, value_reg, init_expr->resolved_type->bit_size);
                            break;
                        }
                    }
                }

            } else {
                assert(stmt->declaration->kind == AST_Declaration_Kind::STRUCT);
            }
            break;
        }

        case AST_Statement_Kind::ASSIGNMENT: {

            switch (stmt->assignment.lvalue->resolved_type->kind) {

                case Type_Kind::INVALID: assert(false); break;
                case Type_Kind::VOID: assert(false); break;

                case Type_Kind::INTEGER:
                case Type_Kind::BOOLEAN: {
                    u32 rvalue = ssa_emit_expression(builder, stmt->assignment.rvalue, scope);
                    auto lvalue = ssa_emit_lvalue(builder, stmt->assignment.lvalue, scope);

                    ssa_emit_store_ptr(builder, stmt->assignment.rvalue->resolved_type->bit_size, lvalue, rvalue);

                    break;
                }

                case Type_Kind::FUNCTION: assert(false); break;

                case Type_Kind::STRUCT: {
                    u32 rvalue = ssa_emit_lvalue(builder, stmt->assignment.rvalue, scope);
                    u32 lvalue = ssa_emit_lvalue(builder, stmt->assignment.lvalue, scope);
                    ssa_emit_memcpy(builder, lvalue, rvalue, stmt->assignment.rvalue->resolved_type->bit_size);
                    break;
                }
            }
            break;
        }

        case AST_Statement_Kind::ARITHMETIC_ASSIGNMENT: {

            auto bit_size = stmt->arithmetic_assignment.lvalue->resolved_type->bit_size;
            u32 lvalue = ssa_emit_lvalue(builder, stmt->arithmetic_assignment.lvalue, scope);
            u32 lhs = ssa_emit_load_ptr(builder, bit_size, lvalue);
            u32 rhs = ssa_emit_expression(builder, stmt->arithmetic_assignment.rvalue, scope);

            switch (stmt->arithmetic_assignment.op) {
                default: assert(false); break;
                case '+': ssa_emit_op(builder, SSA_OP_ADD); break;
                case '/': ssa_emit_op(builder, SSA_OP_DIV); break;
            }

            u32 result = ssa_register_create(builder);
            ssa_emit_32(builder, result);
            ssa_emit_32(builder, lhs);
            ssa_emit_32(builder, rhs);

            ssa_emit_store_ptr(builder, bit_size, lvalue, result);

            break;
        }

        case AST_Statement_Kind::CALL: {
            ssa_emit_expression(builder, stmt->call, scope);
            break;
        }

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_expr) {
                if (builder->function->sret) {
                    u32 src_ptr_reg = ssa_emit_lvalue(builder, stmt->return_expr, scope);
                    u32 dest_ptr_reg = ssa_emit_load_param(builder, 0);
                    ssa_emit_memcpy(builder, dest_ptr_reg, src_ptr_reg, stmt->return_expr->resolved_type->bit_size);

                    ssa_emit_op(builder, SSA_OP_RET);
                    ssa_emit_32(builder, dest_ptr_reg);

                } else {
                    u32 value_reg = ssa_emit_expression(builder, stmt->return_expr, scope);

                    ssa_emit_op(builder, SSA_OP_RET);
                    ssa_emit_32(builder, value_reg);
                }

            } else {
                assert(false);
            }
            break;
        }

        case AST_Statement_Kind::IF: {

            u32 post_if_block = ssa_block_create(builder, "if.post");
            u32 else_block;
            if (stmt->if_stmt.else_stmt) else_block = ssa_block_create(builder, "if.else");
            else else_block = post_if_block;

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {

                auto if_block = stmt->if_stmt.blocks[i];

                u32 true_block = ssa_block_create(builder, "if.true");

                u32 false_block = post_if_block;
                if (i < stmt->if_stmt.blocks.count -1 ) {
                    false_block = ssa_block_create(builder, "if.false");
                } else if (i == stmt->if_stmt.blocks.count - 1 && stmt->if_stmt.else_stmt) {
                    false_block = else_block;
                }

                u32 cond_reg = ssa_emit_expression(builder, if_block.cond, scope);

                assert(!ssa_block_exits(builder, builder->block_index));
                ssa_emit_jmp_if(builder, cond_reg, true_block, false_block);

                ssa_set_insert_point(builder, true_block);
                ssa_emit_statement(builder, if_block.then, scope);
                if (!ssa_block_exits(builder, builder->block_index)) {
                    ssa_emit_jmp(builder, post_if_block);
                }

                ssa_set_insert_point(builder, false_block);
            }

            if (stmt->if_stmt.else_stmt) {
                ssa_set_insert_point(builder, else_block);
                ssa_emit_statement(builder, stmt->if_stmt.else_stmt, scope);
                if (!ssa_block_exits(builder, builder->block_index)) {
                    ssa_emit_jmp(builder, post_if_block);
                }
            }

            ssa_set_insert_point(builder, post_if_block);

            break;
        }

        case AST_Statement_Kind::WHILE: {

            u32 cond_block = ssa_block_create(builder, "while.cond");
            u32 do_block = ssa_block_create(builder, "while.do");
            u32 post_block = ssa_block_create(builder, "while.post");

            ssa_emit_jmp(builder, cond_block);

            ssa_set_insert_point(builder, cond_block);

            u32 cond_reg = ssa_emit_expression(builder, stmt->while_stmt.cond, scope);
            ssa_emit_jmp_if(builder, cond_reg, do_block, post_block);

            ssa_set_insert_point(builder, do_block);

            stack_push(&builder->break_info_stack, { post_block, cond_block });
            ssa_emit_statement(builder, stmt->while_stmt.stmt, scope);
            stack_pop(&builder->break_info_stack);

            ssa_emit_jmp(builder, cond_block);

            ssa_set_insert_point(builder, post_block);
            break;
        }

        case AST_Statement_Kind::FOR: {

            u32 cond_block = ssa_block_create(builder, "for.cond");
            u32 do_block = ssa_block_create(builder, "for.do");
            u32 step_block = ssa_block_create(builder, "for.step");
            u32 post_block = ssa_block_create(builder, "for.post");

            ssa_emit_statement(builder, stmt->for_stmt.init, scope);
            ssa_emit_jmp(builder, cond_block);

            ssa_set_insert_point(builder, cond_block);
            u32 cond = ssa_emit_expression(builder, stmt->for_stmt.cond, scope);
            ssa_emit_jmp_if(builder, cond, do_block, post_block);

            ssa_set_insert_point(builder, do_block);

            stack_push(&builder->break_info_stack, { post_block, step_block });
            ssa_emit_statement(builder, stmt->for_stmt.stmt, scope);
            stack_pop(&builder->break_info_stack);

            ssa_emit_jmp(builder, step_block);

            ssa_set_insert_point(builder, step_block);

            ssa_emit_statement(builder, stmt->for_stmt.step, scope);
            ssa_emit_jmp(builder, cond_block);

            ssa_set_insert_point(builder, post_block);
            break;
        }

        case AST_Statement_Kind::BREAK: {

            assert(stack_count(&builder->break_info_stack));

            SSA_Break_Info break_info = stack_top(&builder->break_info_stack);
            ssa_emit_jmp(builder, break_info.break_block);

            break;
        }

        case AST_Statement_Kind::CONTINUE: {

            assert(stack_count(&builder->break_info_stack));

            SSA_Break_Info break_info = stack_top(&builder->break_info_stack);
            ssa_emit_jmp(builder, break_info.continue_block);

            break;
        }

        case AST_Statement_Kind::BLOCK: {
            Scope *block_scope = stmt->block.scope;
            for (s64 i = 0; i < stmt->block.statements.count; i++) {

                ssa_emit_statement(builder, stmt->block.statements[i], block_scope);
            }
        }
    }
}

u32 ssa_emit_lvalue(SSA_Builder *builder, AST_Expression *lvalue_expr, Scope *scope)
{
    switch (lvalue_expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            assert(lvalue_expr->identifier->decl);
            AST_Declaration *decl = lvalue_expr->identifier->decl;
            assert(decl->kind == AST_Declaration_Kind::VARIABLE);

            bool is_struct = decl->resolved_type->kind == Type_Kind::STRUCT;
            bool is_param = decl->flags & AST_DECL_FLAG_PARAM;

            if (is_param) {
                assert(decl->flags & AST_DECL_FLAG_STORAGE_REQUIRED || is_struct);
            }

            if (is_param && is_struct) {
                u32 param_index = decl->variable.index;
                if (builder->function->sret) param_index++;
                return ssa_emit_load_param(builder, param_index);
            } else {

                u32 alloc_reg;
                bool found = ssa_find_alloc(builder, decl, &alloc_reg);
                assert(found);

                return alloc_reg;
            }
        }

        case AST_Expression_Kind::BINARY: assert(false); break;

        case AST_Expression_Kind::MEMBER: {
            auto base_lvalue = ssa_emit_lvalue(builder, lvalue_expr->member.base, scope);

            u32 result_reg = ssa_register_create(builder);
            ssa_emit_op(builder, SSA_OP_STRUCT_OFFSET);
            ssa_emit_32(builder, result_reg);
            ssa_emit_32(builder, base_lvalue);


            auto field = lvalue_expr->member.member_name->decl;
            assert(field);
            assert(field->kind == AST_Declaration_Kind::STRUCT_MEMBER);

            auto index = field->variable.index;
            assert(index >= 0 && index < U16_MAX);


            Type *struct_type = lvalue_expr->member.base->resolved_type;
            assert(struct_type->kind == Type_Kind::STRUCT);

            auto offset = struct_type->structure.members[index].offset;
            assert(offset % 8 == 0);
            offset = offset / 8;
            assert(offset >= 0 && offset < U32_MAX);


            ssa_emit_32(builder, (u32)offset);
            ssa_emit_16(builder, (u16)index);

            return result_reg;
        }

        case AST_Expression_Kind::CALL: {
            assert(lvalue_expr->resolved_type->kind == Type_Kind::STRUCT);

            return ssa_emit_expression(builder, lvalue_expr, scope);
        }

        case AST_Expression_Kind::INTEGER_LITERAL: assert(false); break;
        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
    }

    assert(false);
}

s64 ssa_emit_expression(SSA_Builder *builder, AST_Expression *expr, Scope *scope)
{
    s64 result = -1;

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            assert(expr->identifier->decl);
            AST_Declaration *decl = expr->identifier->decl;
            assert(decl->kind == AST_Declaration_Kind::VARIABLE);

            if (decl->flags & AST_DECL_FLAG_PARAM) {

                if (decl->flags & AST_DECL_FLAG_STORAGE_REQUIRED) {

                    assert(decl->variable.index >= 0 && decl->variable.index < builder->function->param_count);

                    u32 alloc_reg;
                    bool found = ssa_find_alloc(builder, decl, &alloc_reg);
                    assert(found);

                    result = ssa_emit_load_ptr(builder, decl->resolved_type->bit_size, alloc_reg);

                } else {

                    u32 param_index = decl->variable.index;
                    if (builder->function->sret) param_index++;

                    result = ssa_emit_load_param(builder, param_index);
                }

            } else {

                auto lvalue = ssa_emit_lvalue(builder, expr, scope);
                result = ssa_emit_load_ptr(builder, expr->resolved_type->bit_size, lvalue);
            }
            break;
        }

        case AST_Expression_Kind::BINARY: {
            u32 left = ssa_emit_expression(builder, expr->binary.lhs, scope);
            u32 right = ssa_emit_expression(builder, expr->binary.rhs, scope);

            switch (expr->binary.op) {
                case '+': {
                    ssa_emit_op(builder, SSA_OP_ADD);
                    break;
                }

                case '/': {
                    ssa_emit_op(builder, SSA_OP_DIV);
                    break;
                }

                case '<': {
                    ssa_emit_op(builder, SSA_OP_LT);
                    break;
                }

                case '>': {
                    ssa_emit_op(builder, SSA_OP_GT);
                    break;
                }

                case TOK_EQ: {
                    ssa_emit_op(builder, SSA_OP_EQ);
                    break;
                }

                case TOK_NEQ: {
                    ssa_emit_op(builder, SSA_OP_NEQ);
                    break;
                }

                case TOK_LTEQ: {
                    ssa_emit_op(builder, SSA_OP_LTEQ);
                    break;
                }

                case TOK_GTEQ: {
                    ssa_emit_op(builder, SSA_OP_GTEQ);
                    break;
                }

                default: assert(false);
            }


            result = ssa_register_create(builder);
            ssa_emit_32(builder, result);

            ssa_emit_32(builder, left);
            ssa_emit_32(builder, right);

            break;
        }

        case AST_Expression_Kind::MEMBER: {
            auto lvalue = ssa_emit_lvalue(builder, expr, scope);
            result = ssa_emit_load_ptr(builder, expr->resolved_type->bit_size, lvalue);
            break;
        }

        case AST_Expression_Kind::CALL: {

            // Only support calling via identifier for now...
            assert(expr->call.base->kind == AST_Expression_Kind::IDENTIFIER);
            auto name = expr->call.base->identifier->atom;

            u32 fn_index;
            bool found = ssa_find_function(builder->program, name, &fn_index);
            assert(found);

            bool sret = builder->program->functions[fn_index].sret;

            u32 sret_reg;
            if (sret) {
                bool found = ssa_find_alloc(builder, expr, &sret_reg);
                assert(found);

                ssa_emit_op(builder, SSA_OP_PUSH);
                ssa_emit_32(builder, sret_reg);
            }

            for (s64 i = 0; i < expr->call.args.count; i++) {
                AST_Expression *arg_expr = expr->call.args[i];
                u32 arg_reg;
                if (arg_expr->resolved_type->kind == Type_Kind::STRUCT) {

                    bool found = ssa_find_alloc(builder, arg_expr, &arg_reg);
                    assert(found);

                    u32 src_ptr_reg = ssa_emit_lvalue(builder, arg_expr, scope);
                    ssa_emit_memcpy(builder, arg_reg, src_ptr_reg, arg_expr->resolved_type->bit_size);

                } else {

                    arg_reg = ssa_emit_expression(builder, arg_expr, scope);
                }

                ssa_emit_op(builder, SSA_OP_PUSH);
                ssa_emit_32(builder, arg_reg);
            }

            assert(expr->resolved_type->kind != Type_Kind::VOID);

            ssa_emit_op(builder, SSA_OP_CALL);

            result = ssa_register_create(builder);
            ssa_emit_32(builder, result);

            ssa_emit_32(builder, fn_index);

            u32 arg_pop_count = expr->call.args.count;
            if (sret) arg_pop_count++;

            if (arg_pop_count) {
                ssa_emit_op(builder, SSA_OP_POP_N);
                ssa_emit_32(builder, arg_pop_count);
            }

            if (sret) {
                result = sret_reg;
            }
            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL: {
            ssa_emit_op(builder, SSA_OP_LOAD_IM);

            result = ssa_register_create(builder);
            ssa_emit_32(builder, result);

            ssa_emit_64(builder, expr->integer_literal);
            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;

        case AST_Expression_Kind::BOOL_LITERAL: {
            ssa_emit_op(builder, SSA_OP_LOAD_IM);

            result = ssa_register_create(builder);
            ssa_emit_32(builder, result);
            ssa_emit_64(builder, expr->bool_literal);
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
    }

    return result;
}

u32 ssa_emit_alloc(SSA_Builder *builder, s64 bit_size)
{
    assert(bit_size >= 0);
    assert(bit_size % 8 == 0);
    auto byte_size = bit_size / 8;
    assert(byte_size <= U8_MAX);

    u32 alloc_reg = ssa_register_create(builder);
    ssa_emit_op(builder, SSA_OP_ALLOC);
    ssa_emit_32(builder, alloc_reg);
    ssa_emit_32(builder, byte_size);

    return alloc_reg;
}

void ssa_emit_memcpy(SSA_Builder *builder, u32 dest_ptr_reg, u32 src_ptr_reg, s64 bit_size)
{
    assert(bit_size >= 0);
    assert(bit_size % 8 == 0);
    auto size = bit_size / 8;
    assert(size < U32_MAX);

    ssa_emit_op(builder, SSA_OP_MEMCPY);
    ssa_emit_32(builder, dest_ptr_reg);
    ssa_emit_32(builder, src_ptr_reg);
    ssa_emit_32(builder, size);
}

NAPI void ssa_emit_store_ptr(SSA_Builder *builder, s64 bit_size, u32 dest_reg, u32 source_reg)
{
    assert(bit_size % 8 == 0);
    auto size = bit_size / 8;
    assert(size >= 0 && size <= U8_MAX);

    ssa_emit_op(builder, SSA_OP_STORE_PTR);
    ssa_emit_8(builder, size);
    ssa_emit_32(builder, dest_reg);
    ssa_emit_32(builder, source_reg);
}

u32 ssa_emit_load_param(SSA_Builder *builder, u32 param_index)
{
    u32 result = ssa_register_create(builder);
    ssa_emit_op(builder, SSA_OP_LOAD_PARAM);
    ssa_emit_32(builder, result);
    ssa_emit_32(builder, param_index);

    return result;
}

u32 ssa_emit_load_ptr(SSA_Builder *builder, s64 bit_size, u32 ptr_reg)
{
    assert(bit_size % 8 == 0);
    auto size = bit_size / 8;
    assert(size > 0 && size < U8_MAX);

    u32 dest_reg = ssa_register_create(builder);

    ssa_emit_op(builder, SSA_OP_LOAD_PTR);
    ssa_emit_8(builder, size);
    ssa_emit_32(builder, dest_reg);
    ssa_emit_32(builder, ptr_reg);

    return dest_reg;
}

void ssa_emit_jmp_if(SSA_Builder *builder, u32 cond_reg, u32 true_block, u32 false_block)
{
    assert(!ssa_block_exits(builder, builder->block_index));

    ssa_emit_op(builder, SSA_OP_JMP_IF);
    ssa_emit_32(builder, cond_reg);
    ssa_emit_32(builder, true_block);
    ssa_emit_32(builder, false_block);

    darray_append(&builder->function->blocks[true_block].incoming, (u32)builder->block_index);
    darray_append(&builder->function->blocks[false_block].incoming, (u32)builder->block_index);
}

void ssa_emit_jmp(SSA_Builder *builder, u32 block)
{
    ssa_emit_op(builder, SSA_OP_JMP);
    ssa_emit_32(builder, block);

    darray_append(&builder->function->blocks[block].incoming, (u32)builder->block_index);
}

void ssa_emit_op(SSA_Builder *builder, SSA_Op op)
{
    SSA_Block *block = &builder->function->blocks[builder->block_index];

    assert(!block->exits);

    if (op == SSA_OP_RET ||
        op == SSA_OP_JMP ||
        op == SSA_OP_JMP_IF) {

        assert(!block->exits);

        block->exits = true;

    }

    darray_append(&block->bytes, (u8)op);
}

void ssa_emit_8(SSA_Builder *builder, u8 value)
{
    auto bytes = &builder->function->blocks[builder->block_index].bytes;

    darray_append(bytes, value);
}

void ssa_emit_16(SSA_Builder *builder, u16 value)
{
    auto bytes = &builder->function->blocks[builder->block_index].bytes;

    // Little endian
    darray_append(bytes, (u8)(value & 0xFF));
    darray_append(bytes, (u8)((value >> 8) & 0xFF));
}

void ssa_emit_32(SSA_Builder *builder, u32 value)
{
    auto bytes = &builder->function->blocks[builder->block_index].bytes;

    // Little endian
    darray_append(bytes, (u8)(value & 0xFF));
    darray_append(bytes, (u8)((value >> 8) & 0xFF));
    darray_append(bytes, (u8)((value >> 16) & 0xFF));
    darray_append(bytes, (u8)((value >> 24) & 0xFF));
}

void ssa_emit_64(SSA_Builder *builder, u64 value)
{
    auto bytes = &builder->function->blocks[builder->block_index].bytes;

    // Little endian
    darray_append(bytes, (u8)(value & 0xFF));
    darray_append(bytes, (u8)((value >> 8) & 0xFF));
    darray_append(bytes, (u8)((value >> 16) & 0xFF));
    darray_append(bytes, (u8)((value >> 24) & 0xFF));

    darray_append(bytes, (u8)((value >> 32) & 0xFF));
    darray_append(bytes, (u8)((value >> 40) & 0xFF));
    darray_append(bytes, (u8)((value >> 48) & 0xFF));
    darray_append(bytes, (u8)((value >> 56) & 0xFF));
}

String ssa_to_string(Allocator *allocator, SSA_Program *program)
{
    String_Builder sb;
    string_builder_init(&sb, allocator);

    ssa_print(&sb, program);

    String result = string_builder_to_string(&sb);

    string_builder_free(&sb);

    return result;
}

void ssa_print(String_Builder *sb, SSA_Program *program)
{
    for (s64 fi = 0; fi < program->functions.count; fi++) {
        if (fi != 0) string_builder_append(sb, "\n");

        SSA_Function *fn = &program->functions[fi];
        string_builder_append(sb, "%s:\n", atom_string(fn->name).data);

        s64 block_index = 0;
        int printed_block_count = 0;
        while (block_index >= 0 && block_index < fn->blocks.count) {
            printed_block_count++;
            SSA_Block *block = &fn->blocks[block_index];
            if (block->bytes.count > 0) {

                string_builder_append(sb, " %s:\n", atom_string(block->name).data);

                s64 ip = 0;

                while (ip < block->bytes.count) {
                    ip = ssa_print_instruction(sb, program, fn, ip, block->bytes);
                }

            }

            block_index = block->next_index;
        }

        assert(printed_block_count == fn->blocks.count);
    }
}

s64 ssa_print_instruction(String_Builder *sb, SSA_Program *program, SSA_Function *fn, s64 ip, Array_Ref<u8> bytes)
{
    assert(bytes.count);

    SSA_Op op = (SSA_Op)bytes[ip++];

    switch (op) {

        default: assert(false && "Invalid or unhandled instruction in ssa_print_instruction");

        case SSA_OP_NOP: assert(false); break;

#define BINOP_CASE(op) case SSA_OP_##op: { \
    u32 dest_reg = *(u32 *)&bytes[ip]; \
    ip += sizeof(u32); \
    u32 left = *(u32 *)&bytes[ip]; \
    ip += sizeof(u32); \
    u32 right = *(u32 *)&bytes[ip]; \
    ip += sizeof(u32); \
    string_builder_append(sb, "  %%%u = "#op" %%%u %%%u\n", dest_reg, left, right); \
    break; \
}

        BINOP_CASE(ADD);
        BINOP_CASE(DIV);
        BINOP_CASE(LT);
        BINOP_CASE(GT);
        BINOP_CASE(EQ);
        BINOP_CASE(NEQ);
        BINOP_CASE(LTEQ);
        BINOP_CASE(GTEQ);

#undef BINOP_CASE

        case SSA_OP_ALLOC: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 size = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = ALLOC %u\n", dest_reg, size);
            break;
        }

        case SSA_OP_MEMCPY: {
            u32 dest_ptr_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 source_ptr_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 size = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  MEMCPY %%%u %%%u %u\n", dest_ptr_reg, source_ptr_reg, size);
            break;
        }

        case SSA_OP_STORE_PTR: {
            u8 size_reg = *(u8 *)&bytes[ip];
            ip += sizeof(u8);

            u32 ptr_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 value_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  STORE_PTR %hu %%%u %%%u\n", size_reg, ptr_reg, value_reg);
            break;
        }

        case SSA_OP_LOAD_IM: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u64 value = *(u64 *)&bytes[ip];
            ip += sizeof(u64);

            string_builder_append(sb, "  %%%u = LOAD_IM %llu\n", dest_reg, value);

            break;
        }

        case SSA_OP_LOAD_PARAM: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 index = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = LOAD_PARAM %u\n", dest_reg, index);
            break;
        }

        case SSA_OP_LOAD_PTR: {
            u8 size_reg = *(u8 *)&bytes[ip];
            ip += sizeof(u8);

            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 ptr_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = LOAD_PTR %hu %%%u\n", dest_reg, size_reg, ptr_reg);
            break;
        }

        case SSA_OP_STRUCT_OFFSET: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 ptr_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 offset = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u16 index = *(u16 *)&bytes[ip];
            ip += sizeof(u16);

            string_builder_append(sb, "  %%%u = STRUCT_OFFSET %%%u %u %hu\n", dest_reg, ptr_reg, offset, index);
            break;
        }

        case SSA_OP_PUSH: {
            u32 value_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  PUSH %%%u\n", value_reg);
            break;
        }

        case SSA_OP_POP_N: {
            u32 count = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  POP_N %u\n", count);
            break;
        }

        case SSA_OP_CALL: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 fn_index = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            assert(fn_index >= 0 && fn_index < program->functions.count);
            String name = atom_string(program->functions[fn_index].name);
            string_builder_append(sb, "  %%%u = CALL %%%s\n", dest_reg, name.data);
            break;
        }

        case SSA_OP_RET: {
            u32 value_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  RET %%%u\n", value_reg);
            break;
        }

        case SSA_OP_JMP_IF: {
            u32 cond_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 true_index = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 false_index = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            assert(true_index < fn->blocks.count);
            assert(false_index < fn->blocks.count);

            String true_block_name = atom_string(fn->blocks[true_index].name);
            String false_block_name = atom_string(fn->blocks[false_index].name);

            string_builder_append(sb, "  JMP_IF %%%u [%s] [%s]\n", cond_reg, true_block_name.data, false_block_name.data);
            break;
        }

        case SSA_OP_JMP: {
            u32 block_index = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            String block_name = atom_string(fn->blocks[block_index].name);

            string_builder_append(sb, "  JMP [%s]\n", block_name.data);
            break;
        }
    }

    return ip;
}

}
