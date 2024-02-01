#include "ssa.h"

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

void ssa_program_init(SSA_Program *program, Allocator *allocator)
{
    program->allocator = allocator;
    program->entry_fn_index = -1;
    darray_init(allocator, &program->functions);
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

u32 ssa_register_create(SSA_Function *function)
{
    assert(function->register_count != U32_MAX);
    return function->register_count++;
}

bool ssa_emit_function(Instance *inst, SSA_Program *program, AST_Declaration *decl)
{
    SSA_Function func;
    assert(decl->ident);

    bool sret = decl->resolved_type->function.return_type->kind == Type_Kind::STRUCT;

    ssa_function_init(program, &func, decl->ident->atom, decl->function.params.count, sret);
    s64 block_index = 0;

    auto scope = decl->function.scope;

    // Emit storage for parameters
    for (s64 i = 0; i < decl->function.params.count; i++) {

        auto param_decl = decl->function.params[i];

        if (param_decl->flags & AST_DECL_FLAG_STORAGE_REQUIRED) {
            assert(param_decl->resolved_type->bit_size % 8 == 0);
            auto byte_size = param_decl->resolved_type->bit_size / 8;

            ssa_emit_op(program, &func, &block_index, SSA_OP_ALLOC);
            u32 dest_reg = ssa_register_create(&func);
            ssa_emit_32(program, &func, &block_index, dest_reg);
            ssa_emit_32(program, &func, &block_index, byte_size);

            darray_append(&func.allocs, { ast_node(param_decl), dest_reg });
        }
    }

    // Emit storage for local variables
    for (s64 i = 0; i < decl->function.variables.count; i++) {

        auto var_decl = decl->function.variables[i];
        assert(var_decl->resolved_type->bit_size % 8 == 0);
        auto byte_size = var_decl->resolved_type->bit_size / 8;

        ssa_emit_op(program, &func, &block_index, SSA_OP_ALLOC);
        u32 dest_reg = ssa_register_create(&func);
        ssa_emit_32(program, &func, &block_index, dest_reg);
        ssa_emit_32(program, &func, &block_index, byte_size);

        darray_append(&func.allocs, { ast_node(var_decl), dest_reg });
    }

    // Emit storage for (temporary) aggregates
    for (s64 i = 0; i < decl->function.temp_structs.count; i++) {

        auto expr = decl->function.temp_structs[i];
        assert(expr->resolved_type->bit_size % 8 == 0);
        auto byte_size = expr->resolved_type->bit_size / 8;

        ssa_emit_op(program, &func, &block_index, SSA_OP_ALLOC);
        u32 dest_reg = ssa_register_create(&func);
        ssa_emit_32(program, &func, &block_index, dest_reg);

        ssa_emit_32(program, &func, &block_index, byte_size);

        darray_append(&func.allocs, { ast_node(expr), dest_reg });
    }

    // Copy parameters into local storage
    u32 param_storage_index = 0;
    for (s64 i = 0; i < decl->function.params.count; i++) {
        AST_Declaration *param_decl = decl->function.params[i];

        if (param_decl->flags & AST_DECL_FLAG_STORAGE_REQUIRED) {
            u32 val_reg = ssa_register_create(&func);
            ssa_emit_op(program, &func, &block_index, SSA_OP_LOAD_PARAM);
            ssa_emit_32(program, &func, &block_index, val_reg);
            assert(i <= U32_MAX);

            u32 param_index = i;
            if (sret) param_index++;
            ssa_emit_32(program, &func, &block_index, param_index);

            ssa_emit_op(program, &func, &block_index, SSA_OP_STORE_PTR);
            ssa_emit_32(program, &func, &block_index, param_storage_index++);
            ssa_emit_32(program, &func, &block_index, val_reg);
        }
    }

    // Emit the body
    for (s64 i = 0; i < decl->function.body.count; i++) {
        auto stmt = decl->function.body[i];

        if (ssa_block_exits(&func, &block_index)) {
            auto pos = source_range_start(inst, stmt->range_id);
            instance_fatal_error(inst, pos, "Unreachable code detected");
        }
        ssa_emit_statement(program, &func, &block_index, stmt, scope);
    }

    if (!ssa_block_exits(&func, &block_index) && func.blocks[block_index].incoming.count > 0) {
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

bool ssa_find_alloc(SSA_Function *func, AST_Node *ast_node, u32 *result)
{
    for (s64 i = 0; i < func->allocs.count; i++) {

        if (func->allocs[i].ast_node == *ast_node) {
            *result = func->allocs[i].alloc_reg;
            return true;
        }
    }

    return false;
}

bool ssa_find_alloc(SSA_Function *func, AST_Declaration *decl, u32 *result)
{
    AST_Node node = ast_node(decl);
    return ssa_find_alloc(func, &node, result);
}

bool ssa_find_alloc(SSA_Function *func, AST_Expression *expr, u32 *result)
{
    AST_Node node = ast_node(expr);
    return ssa_find_alloc(func, &node, result);
}

void ssa_set_insert_point(SSA_Function *func, s64 *block_index, u32 new_block_index)
{
    assert(new_block_index >= 0 && new_block_index < func->blocks.count);

    if (*block_index == new_block_index) return;

    func->blocks[*block_index].next_index = new_block_index;
    *block_index = new_block_index;
}

bool ssa_block_exits(SSA_Function *func, s64 *block_index)
{
    s64 index = *block_index;
    assert(index >= 0 && index < func->blocks.count);

    return func->blocks[index].exits;
}

void ssa_emit_statement(SSA_Program *program, SSA_Function *func, s64 *block_index, AST_Statement *stmt, Scope *scope)
{
    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;
        case AST_Statement_Kind::IMPORT: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            if (stmt->declaration->kind == AST_Declaration_Kind::VARIABLE) {

                AST_Expression *init_expr = stmt->declaration->variable.init_expr;
                if (init_expr) {

                    u32 alloc_reg;
                    bool found = ssa_find_alloc(func, stmt->declaration, &alloc_reg);
                    assert(found);

                    switch (init_expr->resolved_type->kind) {

                        case Type_Kind::INVALID: assert(false); break;
                        case Type_Kind::VOID: assert(false); break;

                        case Type_Kind::INTEGER:
                        case Type_Kind::BOOLEAN: {
                            u32 value_reg = ssa_emit_expression(program, func, block_index, init_expr, scope);
                            ssa_emit_op(program, func, block_index, SSA_OP_STORE_PTR);
                            ssa_emit_32(program, func, block_index, alloc_reg);
                            ssa_emit_32(program, func, block_index, value_reg);
                            break;
                        }

                        case Type_Kind::FUNCTION: assert(false); break;

                        case Type_Kind::STRUCT: {
                            u32 value_reg = ssa_emit_lvalue(program, func, block_index, init_expr, scope);
                            ssa_emit_op(program, func, block_index, SSA_OP_MEMCPY);
                            ssa_emit_32(program, func, block_index, alloc_reg);
                            ssa_emit_32(program, func, block_index, value_reg);

                            s64 size = init_expr->resolved_type->bit_size;
                            assert(size % 8 == 0);
                            size /= 8;
                            assert(size >= 0 && size < U32_MAX);

                            ssa_emit_32(program, func, block_index, size);
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
                    u32 rvalue = ssa_emit_expression(program, func, block_index, stmt->assignment.rvalue, scope);
                    auto lvalue = ssa_emit_lvalue(program, func, block_index, stmt->assignment.lvalue, scope);

                    ssa_emit_op(program, func, block_index, SSA_OP_STORE_PTR);
                    ssa_emit_32(program, func, block_index, lvalue);
                    ssa_emit_32(program, func, block_index, rvalue);

                    break;
                }

                case Type_Kind::FUNCTION: assert(false); break;

                case Type_Kind::STRUCT: {
                    u32 rvalue = ssa_emit_lvalue(program, func, block_index, stmt->assignment.rvalue, scope);
                    u32 lvalue = ssa_emit_lvalue(program, func, block_index, stmt->assignment.lvalue, scope);
                    ssa_emit_op(program, func, block_index, SSA_OP_MEMCPY);
                    ssa_emit_32(program, func, block_index, lvalue);
                    ssa_emit_32(program, func, block_index, rvalue);

                    s64 size = stmt->assignment.rvalue->resolved_type->bit_size;
                    assert(size % 8 == 0);
                    size /= 8;
                    assert(size >= 0 && size < U32_MAX);

                    ssa_emit_32(program, func, block_index, size);
                    break;
                }
            }
            break;
        }

        case AST_Statement_Kind::CALL: {
            ssa_emit_expression(program, func, block_index, stmt->call, scope);
            break;
        }

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_expr) {
                if (func->sret) {
                    u32 src_ptr_reg = ssa_emit_lvalue(program, func, block_index, stmt->return_expr, scope);

                    u32 dest_ptr_reg = ssa_register_create(func);
                    ssa_emit_op(program, func, block_index, SSA_OP_LOAD_PARAM);
                    ssa_emit_32(program, func, block_index, dest_ptr_reg);
                    ssa_emit_32(program, func, block_index, 0);

                    ssa_emit_op(program, func, block_index, SSA_OP_MEMCPY);
                    ssa_emit_32(program, func, block_index, dest_ptr_reg);
                    ssa_emit_32(program, func, block_index, src_ptr_reg);

                    s64 size = stmt->return_expr->resolved_type->bit_size;
                    assert(size % 8 == 0);
                    size /= 8;
                    assert(size >= 0 && size < U32_MAX);

                    ssa_emit_32(program, func, block_index, size);

                    ssa_emit_op(program, func, block_index, SSA_OP_RET);
                    ssa_emit_32(program, func, block_index, dest_ptr_reg);

                } else {
                    u32 value_reg = ssa_emit_expression(program, func, block_index, stmt->return_expr, scope);

                    ssa_emit_op(program, func, block_index, SSA_OP_RET);
                    ssa_emit_32(program, func, block_index, value_reg);
                }

            } else {
                assert(false);
            }
            break;
        }

        case AST_Statement_Kind::IF: {

            u32 post_if_block = ssa_block_create(program, func, "if.post");
            u32 else_block;
            if (stmt->if_stmt.else_stmt) else_block = ssa_block_create(program, func, "if.else");
            else else_block = post_if_block;

            for (s64 i = 0; i < stmt->if_stmt.blocks.count; i++) {

                auto if_block = stmt->if_stmt.blocks[i];

                u32 true_block = ssa_block_create(program, func, "if.true");

                u32 false_block = post_if_block;
                if (i < stmt->if_stmt.blocks.count -1 ) {
                    false_block = ssa_block_create(program, func, "if.false");
                } else if (i == stmt->if_stmt.blocks.count - 1 && stmt->if_stmt.else_stmt) {
                    false_block = else_block;
                }

                u32 cond_reg = ssa_emit_expression(program, func, block_index, if_block.cond, scope);

                assert(!ssa_block_exits(func, block_index));
                ssa_emit_jmp_if(program, func, block_index, cond_reg, true_block, false_block);

                ssa_set_insert_point(func, block_index, true_block);
                ssa_emit_statement(program, func, block_index, if_block.then, scope);
                if (!ssa_block_exits(func, block_index)) {
                    ssa_emit_jmp(program, func, block_index, post_if_block);
                }

                ssa_set_insert_point(func, block_index, false_block);
            }

            if (stmt->if_stmt.else_stmt) {
                ssa_set_insert_point(func, block_index, else_block);
                ssa_emit_statement(program, func, block_index, stmt->if_stmt.else_stmt, scope);
                if (!ssa_block_exits(func, block_index)) {
                    ssa_emit_jmp(program, func, block_index, post_if_block);
                }
            }

            ssa_set_insert_point(func, block_index, post_if_block);

            break;
        }

        case AST_Statement_Kind::WHILE: {

            u32 cond_block = ssa_block_create(program, func, "while.cond");
            u32 do_block = ssa_block_create(program, func, "while.do");
            u32 post_block = ssa_block_create(program, func, "while.post");

            ssa_emit_jmp(program, func, block_index, cond_block);

            ssa_set_insert_point(func, block_index, cond_block);

            u32 cond_reg = ssa_emit_expression(program, func, block_index, stmt->while_stmt.cond, scope);
            ssa_emit_jmp_if(program, func, block_index, cond_reg, do_block, post_block);

            ssa_set_insert_point(func, block_index, do_block);

            ssa_emit_statement(program, func, block_index, stmt->while_stmt.stmt, scope);
            ssa_emit_jmp(program, func, block_index, cond_block);

            ssa_set_insert_point(func, block_index, post_block);
            break;
        }

        case AST_Statement_Kind::FOR: {

            u32 cond_block = ssa_block_create(program, func, "for.cond");
            u32 do_block = ssa_block_create(program, func, "for.do");
            u32 post_block = ssa_block_create(program, func, "for.post");

            ssa_emit_statement(program, func, block_index, stmt->for_stmt.init, scope);
            ssa_emit_jmp(program, func, block_index, cond_block);

            ssa_set_insert_point(func, block_index, cond_block);
            u32 cond = ssa_emit_expression(program, func, block_index, stmt->for_stmt.cond, scope);
            ssa_emit_jmp_if(program, func, block_index, cond, do_block, post_block);

            ssa_set_insert_point(func, block_index, do_block);
            ssa_emit_statement(program, func, block_index, stmt->for_stmt.stmt, scope);
            ssa_emit_statement(program, func, block_index, stmt->for_stmt.step, scope);
            ssa_emit_jmp(program, func, block_index, cond_block);

            ssa_set_insert_point(func, block_index, post_block);
            break;
        }

        case AST_Statement_Kind::BLOCK: {
            Scope *block_scope = stmt->block.scope;
            for (s64 i = 0; i < stmt->block.statements.count; i++) {

                ssa_emit_statement(program, func, block_index, stmt->block.statements[i], block_scope);
            }
        }
    }
}

u32 ssa_emit_lvalue(SSA_Program *program, SSA_Function *func, s64 *block_index, AST_Expression *lvalue_expr, Scope *scope)
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
                ssa_emit_op(program, func, block_index, SSA_OP_LOAD_PARAM);
                u32 result_reg = ssa_register_create(func);
                ssa_emit_32(program, func, block_index, result_reg);

                u32 param_index = decl->variable.index;
                if (func->sret) param_index++;
                ssa_emit_32(program, func, block_index, param_index);
                return result_reg;
            } else {

                u32 alloc_reg;
                bool found = ssa_find_alloc(func, decl, &alloc_reg);
                assert(found);

                return alloc_reg;
            }
        }

        case AST_Expression_Kind::BINARY: assert(false); break;

        case AST_Expression_Kind::MEMBER: {
            auto base_lvalue = ssa_emit_lvalue(program, func, block_index, lvalue_expr->member.base, scope);

            u32 result_reg = ssa_register_create(func);
            ssa_emit_op(program, func, block_index, SSA_OP_STRUCT_OFFSET);
            ssa_emit_32(program, func, block_index, result_reg);
            ssa_emit_32(program, func, block_index, base_lvalue);


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


            ssa_emit_32(program, func, block_index, (u32)offset);
            ssa_emit_16(program, func, block_index, (u16)index);

            return result_reg;
        }

        case AST_Expression_Kind::CALL: {
            assert(lvalue_expr->resolved_type->kind == Type_Kind::STRUCT);

            return ssa_emit_expression(program, func, block_index, lvalue_expr, scope);
        }

        case AST_Expression_Kind::INTEGER_LITERAL: assert(false); break;
        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
    }

    assert(false);
}

s64 ssa_emit_expression(SSA_Program *program, SSA_Function *func, s64 *block_index, AST_Expression *expr, Scope *scope)
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

                    assert(decl->variable.index >= 0 && decl->variable.index < func->param_count);
                    result = ssa_register_create(func);
                    ssa_emit_op(program, func, block_index, SSA_OP_LOAD_PTR);
                    ssa_emit_32(program, func, block_index, result);

                    u32 alloc_reg;
                    bool found = ssa_find_alloc(func, decl, &alloc_reg);
                    assert(found);

                    ssa_emit_32(program, func, block_index, alloc_reg);
                } else {

                    result = ssa_register_create(func);
                    ssa_emit_op(program, func, block_index, SSA_OP_LOAD_PARAM);
                    ssa_emit_32(program, func, block_index, result);

                    u32 param_index = decl->variable.index;
                    if (func->sret) param_index++;
                    ssa_emit_32(program, func, block_index, param_index);
                }

            } else {

                auto lvalue = ssa_emit_lvalue(program, func, block_index, expr, scope);

                result = ssa_register_create(func);

                ssa_emit_op(program, func, block_index, SSA_OP_LOAD_PTR);

                ssa_emit_32(program, func, block_index, result);
                ssa_emit_32(program, func, block_index, lvalue);
            }
            break;
        }

        case AST_Expression_Kind::BINARY: {
            u32 left = ssa_emit_expression(program, func, block_index, expr->binary.lhs, scope);
            u32 right = ssa_emit_expression(program, func, block_index, expr->binary.rhs, scope);

            switch (expr->binary.op) {
                case '+': {
                    ssa_emit_op(program, func, block_index, SSA_OP_ADD);
                    break;
                }

                case '/': {
                    ssa_emit_op(program, func, block_index, SSA_OP_DIV);
                    break;
                }

                case '<': {
                    ssa_emit_op(program, func, block_index, SSA_OP_LT);
                    break;
                }

                case TOK_EQ: {
                    ssa_emit_op(program, func, block_index, SSA_OP_EQ);
                    break;
                }

                default: assert(false);
            }


            result = ssa_register_create(func);
            ssa_emit_32(program, func, block_index, result);

            ssa_emit_32(program, func, block_index, left);
            ssa_emit_32(program, func, block_index, right);

            break;
        }

        case AST_Expression_Kind::MEMBER: {
            auto lvalue = ssa_emit_lvalue(program, func, block_index, expr, scope);

            result = ssa_register_create(func);
            ssa_emit_op(program, func, block_index, SSA_OP_LOAD_PTR);
            ssa_emit_32(program, func, block_index, result);
            ssa_emit_32(program, func, block_index, lvalue);
            break;
        }

        case AST_Expression_Kind::CALL: {

            // Only support calling via identifier for now...
            assert(expr->call.base->kind == AST_Expression_Kind::IDENTIFIER);
            auto name = expr->call.base->identifier->atom;

            u32 fn_index;
            bool found = ssa_find_function(program, name, &fn_index);
            assert(found);

            bool sret = program->functions[fn_index].sret;

            u32 sret_reg;
            if (sret) {
                bool found = ssa_find_alloc(func, expr, &sret_reg);
                assert(found);

                ssa_emit_op(program, func, block_index, SSA_OP_PUSH);
                ssa_emit_32(program, func, block_index, sret_reg);
            }

            for (s64 i = 0; i < expr->call.args.count; i++) {
                AST_Expression *arg_expr = expr->call.args[i];
                u32 arg_reg;
                if (arg_expr->resolved_type->kind == Type_Kind::STRUCT) {

                    bool found = ssa_find_alloc(func, arg_expr, &arg_reg);
                    assert(found);

                    u32 src_ptr_reg = ssa_emit_lvalue(program, func, block_index, arg_expr, scope);
                    ssa_emit_op(program, func, block_index, SSA_OP_MEMCPY);
                    ssa_emit_32(program, func, block_index, arg_reg);
                    ssa_emit_32(program, func, block_index, src_ptr_reg);

                    s64 size = arg_expr->resolved_type->bit_size;
                    assert(size % 8 == 0);
                    size /= 8;
                    assert(size >= 0 && size < U32_MAX);

                    ssa_emit_32(program, func, block_index, size);
                } else {
                    arg_reg = ssa_emit_expression(program, func, block_index, arg_expr, scope);
                }

                ssa_emit_op(program, func, block_index, SSA_OP_PUSH);
                ssa_emit_32(program, func, block_index, arg_reg);
            }

            assert(expr->resolved_type->kind != Type_Kind::VOID);

            ssa_emit_op(program, func, block_index, SSA_OP_CALL);

            result = ssa_register_create(func);
            ssa_emit_32(program, func, block_index, result);

            ssa_emit_32(program, func, block_index, fn_index);

            u32 arg_pop_count = expr->call.args.count;
            if (sret) arg_pop_count++;

            if (arg_pop_count) {
                ssa_emit_op(program, func, block_index, SSA_OP_POP_N);
                ssa_emit_32(program, func, block_index, arg_pop_count);
            }

            if (sret) {
                result = sret_reg;
            }
            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL: {
            ssa_emit_op(program, func, block_index, SSA_OP_LOAD_IM);

            result = ssa_register_create(func);
            ssa_emit_32(program, func, block_index, result);

            ssa_emit_64(program, func, block_index, expr->integer_literal);
            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;

        case AST_Expression_Kind::BOOL_LITERAL: {
            ssa_emit_op(program, func, block_index, SSA_OP_LOAD_IM);

            result = ssa_register_create(func);
            ssa_emit_32(program, func, block_index, result);
            ssa_emit_64(program, func, block_index, expr->bool_literal);
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
    }

    return result;
}

void ssa_emit_jmp_if(SSA_Program *program, SSA_Function *func, s64 *block_index, u32 cond_reg, u32 true_block, u32 false_block)
{
    ssa_emit_op(program, func, block_index, SSA_OP_JMP_IF);
    ssa_emit_32(program, func, block_index, cond_reg);
    ssa_emit_32(program, func, block_index, true_block);
    ssa_emit_32(program, func, block_index, false_block);

    darray_append(&func->blocks[true_block].incoming, (u32)*block_index);
    darray_append(&func->blocks[false_block].incoming, (u32)*block_index);
}

void ssa_emit_jmp(SSA_Program *program, SSA_Function *func, s64 *block_index, u32 block)
{
    ssa_emit_op(program, func, block_index, SSA_OP_JMP);
    ssa_emit_32(program, func, block_index, block);

    darray_append(&func->blocks[block].incoming, (u32)*block_index);
}

void ssa_emit_op(SSA_Program *program, SSA_Function *func, s64 *block_index, SSA_Op op)
{
    SSA_Block *block = &func->blocks[*block_index];

    assert(!block->exits);

    if (op == SSA_OP_RET ||
        op == SSA_OP_JMP ||
        op == SSA_OP_JMP_IF) {

        assert(!block->exits);

        block->exits = true;

    }

    darray_append(&block->bytes, (u8)op);
}

void ssa_emit_8(SSA_Program *program, SSA_Function *func, s64 *block_index, u8 value)
{
    auto bytes = &func->blocks[*block_index].bytes;

    darray_append(bytes, value);
}

void ssa_emit_16(SSA_Program *program, SSA_Function *func, s64 *block_index, u16 value)
{
    auto bytes = &func->blocks[*block_index].bytes;

    // Little endian
    darray_append(bytes, (u8)(value & 0xFF));
    darray_append(bytes, (u8)((value >> 8) & 0xFF));
}

void ssa_emit_32(SSA_Program *program, SSA_Function *func, s64 *block_index, u32 value)
{
    auto bytes = &func->blocks[*block_index].bytes;

    // Little endian
    darray_append(bytes, (u8)(value & 0xFF));
    darray_append(bytes, (u8)((value >> 8) & 0xFF));
    darray_append(bytes, (u8)((value >> 16) & 0xFF));
    darray_append(bytes, (u8)((value >> 24) & 0xFF));
}

void ssa_emit_64(SSA_Program *program, SSA_Function *func, s64 *block_index, u64 value)
{
    auto bytes = &func->blocks[*block_index].bytes;

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

        case SSA_OP_ADD: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 left = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 right = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = ADD %%%u %%%u\n", dest_reg, left, right);
            break;
        }

        case SSA_OP_DIV: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 left = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 right = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = DIV %%%u %%%u\n", dest_reg, left, right);
            break;
        }

        case SSA_OP_LT: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 left = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 right = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = LT %%%u %%%u\n", dest_reg, left, right);
            break;
        }

        case SSA_OP_EQ: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 left = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 right = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = EQ %%%u %%%u\n", dest_reg, left, right);
            break;
        }

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
            u32 ptr_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 value_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  STORE_PTR %%%u %%%u\n", ptr_reg, value_reg);
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
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 ptr_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = LOAD_PTR %%%u\n", dest_reg, ptr_reg);
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
