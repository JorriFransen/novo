#include "ssa.h"

#include <containers/darray.h>
#include <containers/stack.h>
#include <defines.h>
#include <hash.h>
#include <memory/arena.h>
#include <string_builder.h>

#include "ast.h"
#include "instance.h"
#include "token.h"
#include "type.h"

#include <assert.h>
#include <string.h>

namespace Novo {

struct SSA_Break_Info
{
    u32 break_block;
    u32 continue_block;
};

struct SSA_Builder
{
    Instance *instance;
    SSA_Program* program;

    s64 function_index;
    s64 block_index;

    Stack<SSA_Break_Info> break_info_stack;
};

u64 hash_key(SSA_Assert_Pos key)
{
    u64 r = hash_combine(key.block_index, key.offset);
    r = hash_combine(key.fn_index, key.offset);
    return r;
}

bool operator==(const SSA_Assert_Pos& l, const SSA_Assert_Pos& r)
{
    return l.offset == r.offset && l.fn_index == r.fn_index;
}

void ssa_program_init(SSA_Program* program, Allocator* allocator)
{
    program->allocator = allocator;
    program->entry_fn_index = -1;
    darray_init(allocator, &program->constant_memory);
    darray_init(allocator, &program->constants);
    darray_init(allocator, &program->constant_patch_offsets);
    darray_init(allocator, &program->functions);
    darray_init(allocator, &program->constant_references);
    darray_init(allocator, &program->globals);
    program->globals_size = 0;
    hash_table_create(allocator, &program->instruction_origin_positions);
}

void ssa_program_free(SSA_Program* program)
{
    for (s64 fi = 0; fi < program->functions.count; fi++) {
        SSA_Function* func = &program->functions[fi];

        for (s64 bi = 0; bi < func->blocks.count; bi++) {
            SSA_Block* block = &func->blocks[bi];

            darray_free(&block->bytes);
            darray_free(&block->incoming);
        }

        darray_free(&func->blocks);
        darray_free(&func->allocs);
        darray_free(&func->registers);

    }

    darray_free(&program->constant_memory);
    darray_free(&program->constants);
    darray_free(&program->constant_patch_offsets);
    darray_free(&program->functions);
    darray_free(&program->constant_references);
    darray_free(&program->globals);

    hash_table_free(&program->instruction_origin_positions);
}

void ssa_function_init(Instance* inst, SSA_Program* program, SSA_Function* func, AST_Declaration *decl)
{
    bool foreign = decl->flags & AST_DECL_FLAG_FOREIGN;
    Source_Pos pos = source_pos(inst, decl);
    ssa_function_init(inst, program, func, decl->resolved_type, decl->ident->atom, foreign, pos);
}

void ssa_function_init(Instance* inst, SSA_Program* program, SSA_Function* func, Type* type, Atom name, bool foreign, Source_Pos source_pos)
{
    assert(type->kind == Type_Kind::FUNCTION);

    for (s64 i = 0; i < program->functions.count; i++) {
        assert(program->functions[i].name != name && "Duplicate ssa function name!");
    }

    func->name = name;
    func->register_count = 0;
    func->param_count = type->function.param_types.count;
    func->type = type;
    darray_init(program->allocator, &func->blocks);
    darray_init(program->allocator, &func->allocs);
    darray_init(program->allocator, &func->registers, 0);
    func->total_alloc_size = 0;

    if (!foreign) ssa_block_create(program, func, "entry");

    bool sret = type->function.return_type->kind == Type_Kind::STRUCT;

    func->sret = sret;
    if (sret) func->param_count++;

    func->foreign = foreign;
    func->run_wrapper = false;
    func->source_pos = source_pos;
}

void ssa_block_init(SSA_Program* program, SSA_Function* func, SSA_Block* block, Atom name)
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

void ssa_block_init(SSA_Program* program, SSA_Function* func, SSA_Block* block, const char* name)
{
    return ssa_block_init(program, func, block, atom_get(name));
}

u32 ssa_block_create(SSA_Program* program, SSA_Function* function, const char* name)
{
    SSA_Block result;
    ssa_block_init(program, function, &result, name);
    s64 index = function->blocks.count;
    assert(index >= 0 && U32_MAX);

    darray_append(&function->blocks, result);
    return (u32)index;
}

void ssa_global_variable_init(Instance *inst, SSA_Program* program, SSA_Global* glob, AST_Declaration* decl, u64 offset)
{
    Source_Pos pos = source_pos(inst, decl);

    assert(decl->variable.init_expr);

    u32 init_index = ssa_emit_constant(inst, program, decl->variable.init_expr);

    ssa_global_variable_init(glob, decl->resolved_type, decl->ident->atom, init_index, pos, offset);
}

void ssa_global_variable_init(SSA_Global* glob, Type* type, Atom name, u32 init_const_index, Source_Pos source_pos, u64 offset)
{
    glob->name = name;
    glob->type = type;
    glob->initializer_constant_index = init_const_index;
    glob->offset = offset;
    glob->source_pos = source_pos;
}

u32 ssa_block_create(SSA_Builder* builder, const char* name)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];
    return ssa_block_create(builder->program, function, name);
}

SSA_Register_Handle ssa_register_create(SSA_Builder* builder, Type* type, bool alloc_reg/*= false*/)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];
    assert(function->register_count != U32_MAX);
    darray_append(&function->registers, { type, false, alloc_reg });
    return { function->register_count++ };
}

static int cmp_decl_order(AST_Declaration* a, AST_Declaration* b) {
    return (int)(a->variable.index - b->variable.index);
}

bool ssa_emit_function(Instance* inst, SSA_Program* program, AST_Declaration* decl)
{
    assert(decl->kind == AST_Declaration_Kind::FUNCTION);
    assert(decl->resolved_type);
    assert(decl->resolved_type->kind == Type_Kind::FUNCTION);

    SSA_Function local_func;
    assert(decl->ident);

    bool sret = decl->resolved_type->function.return_type->kind == Type_Kind::STRUCT;

    ssa_function_init(inst, program, &local_func, decl);

    s64 fn_index = program->functions.count;
    if (decl->ident->atom == atom_get("main")) {
        assert(program->entry_fn_index == -1);
        program->entry_fn_index = fn_index;
    }
    darray_append(&program->functions, local_func);
    SSA_Function* func = &program->functions[fn_index];

    if (func->foreign) {
        return true;
    }

    SSA_Builder local_builder;
    local_builder.instance = inst;
    local_builder.program = program;
    local_builder.function_index = fn_index;
    local_builder.block_index = 0;

    Temp_Arena tarena = temp_arena(nullptr);
    Allocator ta = arena_allocator_create(tarena.arena);

    stack_init(&ta, &local_builder.break_info_stack, 0);

    SSA_Builder* builder = &local_builder;

    auto scope = decl->function.scope;

    // Emit storage for parameters
    for (s64 i = 0; i < decl->function.params.count; i++) {

        auto param_decl = decl->function.params[i];

        if (param_decl->flags & AST_DECL_FLAG_PARAMETER_STORAGE_REQUIRED) {

            SSA_Register_Handle alloc_reg = ssa_emit_alloc(builder, param_decl->resolved_type);
            darray_append(&func->allocs, { ast_node(param_decl), alloc_reg });
        }
    }

    quicksort(Array_Ref(decl->function.variables), cmp_decl_order);

    // Emit storage for local variables
    for (s64 i = 0; i < decl->function.variables.count; i++) {

        auto var_decl = decl->function.variables[i];

        SSA_Register_Handle alloc_reg = ssa_emit_alloc(builder, var_decl->resolved_type);
        darray_append(&func->allocs, { ast_node(var_decl), alloc_reg });
    }

    // Emit storage for (temporary) aggregates
    for (s64 i = 0; i < decl->function.temp_structs.count; i++) {

        auto expr = decl->function.temp_structs[i];

        SSA_Register_Handle alloc_reg = ssa_emit_alloc(builder, expr->resolved_type);
        darray_append(&func->allocs, { ast_node(expr), alloc_reg });
    }

    // Copy parameters into local storage
    u32 param_storage_index = 0;
    for (s64 i = 0; i < decl->function.params.count; i++) {
        AST_Declaration* param_decl = decl->function.params[i];

        if (param_decl->flags & AST_DECL_FLAG_PARAMETER_STORAGE_REQUIRED) {

            u32 param_index = i;
            if (sret) param_index++;
            SSA_Register_Handle param_reg = ssa_emit_load_param(builder, param_index);

            SSA_Register_Handle param_storage_reg = { param_storage_index++ };

            ssa_emit_store_ptr(builder, param_decl->resolved_type->bit_size, param_storage_reg, param_reg);
        }
    }

    // Emit the body
    for (s64 i = 0; i < decl->function.body.count; i++) {
        auto stmt = decl->function.body[i];

        if (ssa_block_exits(builder, builder->block_index)) {
            Source_Pos pos = source_pos(inst, stmt);
            instance_fatal_error(inst, pos, "Unreachable code detected");
        }
        ssa_emit_statement(builder, stmt, scope);
    }

    bool last_block_exits = ssa_block_exits(builder, builder->block_index);

    if (!last_block_exits) {
        if (decl->resolved_type->function.return_type->kind == Type_Kind::VOID) {
            ssa_emit_op(builder, SSA_OP_RET_VOID);
        } else if (builder->block_index == 0 || func->blocks[builder->block_index].incoming.count > 0) {
            instance_fatal_error(inst, func->source_pos, "Function '%s' does not return a value from all control paths", atom_string(func->name).data);
        }
    }

    temp_arena_release(tarena);

    return true;
}

bool ssa_emit_global_variable(Instance* inst, SSA_Program* program, AST_Declaration* decl)
{
    assert(decl->kind == AST_Declaration_Kind::VARIABLE);
    assert(decl->flags & AST_DECL_FLAG_GLOBAL);

    s64 global_size = decl->resolved_type->bit_size;
    assert(global_size % 8 == 0);

    program->globals_size = get_aligned(program->globals_size, decl->resolved_type->alignment * 8);
    u64 offset = program->globals_size;
    program->globals_size += global_size;

    SSA_Global global;
    assert(decl->ident);
    ssa_global_variable_init(inst, program, &global, decl, offset);

    darray_append(&program->globals, global);

    return true;
}

s64 ssa_emit_run_wrapper(Instance* inst, SSA_Program* program, AST_Node node, Scope* scope)
{
    AST_Expression* expr = nullptr;
    if (node.kind == AST_Node_Kind::EXPRESSION) {
        expr = node.expression->run.expression;
    } else {
        assert(node.kind == AST_Node_Kind::STATEMENT);
        assert(node.statement->kind == AST_Statement_Kind::RUN ||
               node.statement->kind == AST_Statement_Kind::INSERT);

        expr = node.statement->run.expression;
    }

    assert(expr);
    assert(expr->kind == AST_Expression_Kind::CALL);

    Type* called_fn_type = expr->call.base->resolved_type;
    assert(called_fn_type->kind == Type_Kind::FUNCTION);

    Type* return_type = called_fn_type->function.return_type;

    Type* wrapper_fn_type = function_type_get(inst, {}, return_type, TYPE_FLAG_NONE);

    Source_Pos pos = source_pos(inst, node);
    Imported_File file = inst->imported_files[pos.file_index];

    Temp_Arena tarena = temp_arena(nullptr);
    Allocator ta = arena_allocator_create(tarena.arena);

    String initial_name = string_format(&ta, "run_wrapper.%s.%u.%u", atom_string(file.name).data, pos.offset, pos.length);
    Atom name = ssa_unique_function_name(inst, program, initial_name);

    temp_arena_release(tarena);

    SSA_Function local_func;
    s64 fn_index = program->functions.count;
    ssa_function_init(inst, program, &local_func, wrapper_fn_type, name, false, pos);
    local_func.run_wrapper = true;

    darray_append(&program->functions, local_func);
    SSA_Function* func = &program->functions[fn_index];


    SSA_Builder local_builder;
    local_builder.instance = inst;
    local_builder.program = program;
    local_builder.function_index = fn_index;
    local_builder.block_index = 0;

    stack_init(&ta, &local_builder.break_info_stack, 0);

    SSA_Builder* builder = &local_builder;

    bool sret = return_type->kind == Type_Kind::STRUCT;
    SSA_Register_Handle sret_alloc_reg;

    if (sret) {
        sret_alloc_reg = ssa_emit_alloc(builder, return_type);
        darray_append(&func->allocs, { ast_node(expr), sret_alloc_reg });
    }

    for (s64 i = 0; i < expr->call.args.count; i++) {
        AST_Expression* arg_expr = expr->call.args[i];

        if (arg_expr->resolved_type->kind == Type_Kind::STRUCT) {
            SSA_Register_Handle alloc_reg = ssa_emit_alloc(builder, arg_expr->resolved_type);
            darray_append(&func->allocs, { ast_node(arg_expr), alloc_reg });
        }
    }

    SSA_Register_Handle result_reg = ssa_emit_expression(builder, expr, scope);
    ssa_emit_op(builder, SSA_OP_RET);

    if (sret) {
        ssa_emit_reg(builder, sret_alloc_reg);
    } else {
        ssa_emit_reg(builder, result_reg);
    }

    temp_arena_release(tarena);

    return fn_index;
}

bool ssa_find_function(SSA_Program* program, Atom atom, u32* index)
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

bool ssa_find_global_variable(SSA_Program* program, Atom atom, u32* index)
{
    bool found = false;
    for (s64 i = 0; i < program->globals.count; i++) {
        if (program->globals[i].name == atom) {
            found = true;
            if (index) *index = i;
            break;
        }
    }

    return found;
}

bool ssa_find_alloc(SSA_Builder* builder, AST_Node* ast_node, SSA_Register_Handle* result)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];

    for (s64 i = 0; i < function->allocs.count; i++) {

        if (function->allocs[i].ast_node == *ast_node) {
            *result = function->allocs[i].alloc_reg;
            return true;
        }
    }

    return false;
}

bool ssa_find_alloc(SSA_Builder* builder, AST_Declaration* decl, SSA_Register_Handle* result)
{
    AST_Node node = ast_node(decl);
    return ssa_find_alloc(builder, &node, result);
}

bool ssa_find_alloc(SSA_Builder* builder, AST_Expression* expr, SSA_Register_Handle* result)
{
    AST_Node node = ast_node(expr);
    return ssa_find_alloc(builder, &node, result);
}

void ssa_set_insert_point(SSA_Builder* builder, u32 new_block_index)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];

    assert(new_block_index >= 0 && new_block_index < function->blocks.count);

    if (builder->block_index == new_block_index) return;

    function->blocks[builder->block_index].next_index = new_block_index;
    builder->block_index = new_block_index;
}

bool ssa_block_exits(SSA_Builder* builder, s64 block_index)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];

    assert(block_index >= 0 && block_index < function->blocks.count);

    return function->blocks[block_index].exits;
}

void ssa_emit_statement(SSA_Builder* builder, AST_Statement* stmt, Scope* scope)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];

    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;
        case AST_Statement_Kind::IMPORT: assert(false); break;

        case AST_Statement_Kind::DECLARATION: {
            if (stmt->declaration->kind == AST_Declaration_Kind::VARIABLE) {

                AST_Expression* init_expr = stmt->declaration->variable.init_expr;
                if (init_expr) {

                    SSA_Register_Handle alloc_reg;
                    bool found = ssa_find_alloc(builder, stmt->declaration, &alloc_reg);
                    assert(found);

                    switch (init_expr->resolved_type->kind) {

                        case Type_Kind::INVALID: assert(false); break;
                        case Type_Kind::VOID: assert(false); break;

                        case Type_Kind::INTEGER:
                        case Type_Kind::BOOLEAN:
                        case Type_Kind::POINTER:
                        case Type_Kind::ENUM: {
                            SSA_Register_Handle value_reg = ssa_emit_expression(builder, init_expr, scope);
                            ssa_emit_store_ptr(builder, init_expr->resolved_type->bit_size, alloc_reg, value_reg);
                            break;
                        }

                        case Type_Kind::FUNCTION: assert(false); break;

                        case Type_Kind::ARRAY:
                        case Type_Kind::STRUCT: {
                            SSA_Register_Handle value_reg = ssa_emit_lvalue(builder, init_expr, scope);
                            ssa_emit_memcpy(builder, alloc_reg, value_reg, init_expr->resolved_type->bit_size);
                            break;
                        }
                    }
                }

            } else if (stmt->declaration->kind == AST_Declaration_Kind::CONSTANT) {
                // No init required, emitted when used (referenced)
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
                case Type_Kind::BOOLEAN:
                case Type_Kind::POINTER: {
                    SSA_Register_Handle rvalue = ssa_emit_expression(builder, stmt->assignment.rvalue, scope);
                    auto lvalue = ssa_emit_lvalue(builder, stmt->assignment.lvalue, scope);

                    ssa_emit_store_ptr(builder, stmt->assignment.rvalue->resolved_type->bit_size, lvalue, rvalue);

                    break;
                }

                case Type_Kind::ARRAY: assert(false); break;
                case Type_Kind::FUNCTION: assert(false); break;

                case Type_Kind::STRUCT: {
                    SSA_Register_Handle rvalue = ssa_emit_lvalue(builder, stmt->assignment.rvalue, scope);
                    SSA_Register_Handle lvalue = ssa_emit_lvalue(builder, stmt->assignment.lvalue, scope);
                    ssa_emit_memcpy(builder, lvalue, rvalue, stmt->assignment.rvalue->resolved_type->bit_size);
                    break;
                }

                case Type_Kind::ENUM: assert(false); break;
            }
            break;
        }

        case AST_Statement_Kind::ARITHMETIC_ASSIGNMENT: {

            auto bit_size = stmt->arithmetic_assignment.lvalue->resolved_type->bit_size;
            AST_Expression* lvalue_expr = stmt->arithmetic_assignment.lvalue;
            AST_Expression* rvalue_expr = stmt->arithmetic_assignment.rvalue;

            u32 op = stmt->arithmetic_assignment.op;

            if (lvalue_expr->resolved_type->kind == Type_Kind::POINTER) {
                assert(bit_size == 64); // pointer size

                SSA_Register_Handle lvalue = ssa_emit_lvalue(builder, stmt->arithmetic_assignment.lvalue, scope);
                SSA_Register_Handle left_reg = ssa_emit_load_ptr(builder, stmt->arithmetic_assignment.lvalue->resolved_type, lvalue);
                SSA_Register_Handle right_reg = ssa_emit_expression(builder, stmt->arithmetic_assignment.rvalue, scope);

                assert(rvalue_expr->resolved_type->kind == Type_Kind::INTEGER);

                if (op == '-') {
                    SSA_Register_Handle zero_reg = ssa_emit_load_immediate(builder, rvalue_expr->resolved_type, 0);
                    SSA_Register_Handle new_right = ssa_register_create(builder, rvalue_expr->resolved_type);

                    // TODO:  Negate op?
                    ssa_emit_op(builder, SSA_OP_SUB);
                    ssa_emit_8(builder, bit_size / 8);
                    ssa_emit_reg(builder, new_right);
                    ssa_emit_reg(builder, zero_reg);
                    ssa_emit_reg(builder, right_reg);

                    right_reg = new_right;
                }

                SSA_Register_Handle new_ptr_reg = ssa_emit_pointer_offset(builder, lvalue_expr->resolved_type, left_reg, right_reg);
                ssa_emit_store_ptr(builder, bit_size, lvalue, new_ptr_reg);


            } else {
                SSA_Register_Handle lvalue = ssa_emit_lvalue(builder, lvalue_expr, scope);
                SSA_Register_Handle lhs = ssa_emit_load_ptr(builder, stmt->arithmetic_assignment.lvalue->resolved_type, lvalue);
                SSA_Register_Handle rhs = ssa_emit_expression(builder, rvalue_expr, scope);

                switch (op) {
                    default: assert(false); break;
                    case '+': ssa_emit_op(builder, SSA_OP_ADD); break;
                    case '-': ssa_emit_op(builder, SSA_OP_SUB); break;
                    case '*': ssa_emit_op(builder, SSA_OP_MUL); break;
                    case '/': ssa_emit_op(builder, SSA_OP_DIV); break;
                }

                assert(bit_size % 8 == 0);
                auto size = bit_size / 8;
                assert(size >= 0 && size <= U8_MAX);
                ssa_emit_8(builder, (u8)size);

                SSA_Register_Handle result = ssa_register_create(builder, lvalue_expr->resolved_type);
                ssa_emit_reg(builder, result);
                ssa_emit_reg(builder, lhs);
                ssa_emit_reg(builder, rhs);

                ssa_emit_store_ptr(builder, bit_size, lvalue, result);
            }

            break;
        }

        case AST_Statement_Kind::CALL: {
            ssa_emit_expression(builder, stmt->call, scope);
            break;
        }

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_expr) {
                if (function->sret) {
                    SSA_Register_Handle src_ptr_reg = ssa_emit_lvalue(builder, stmt->return_expr, scope);
                    SSA_Register_Handle dest_ptr_reg = ssa_emit_load_param(builder, 0);
                    ssa_emit_memcpy(builder, dest_ptr_reg, src_ptr_reg, stmt->return_expr->resolved_type->bit_size);

                    ssa_emit_op(builder, SSA_OP_RET);
                    ssa_emit_reg(builder, dest_ptr_reg);

                } else {
                    SSA_Register_Handle value_reg = ssa_emit_expression(builder, stmt->return_expr, scope);

                    ssa_emit_op(builder, SSA_OP_RET);
                    ssa_emit_reg(builder, value_reg);
                }

            } else {

                ssa_emit_op(builder, SSA_OP_RET_VOID);
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

                SSA_Register_Handle cond_reg = ssa_emit_expression(builder, if_block.cond, scope);

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

            SSA_Register_Handle cond_reg = ssa_emit_expression(builder, stmt->while_stmt.cond, scope);
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
            SSA_Register_Handle cond = ssa_emit_expression(builder, stmt->for_stmt.cond, scope);
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

        case AST_Statement_Kind::RUN: assert(false); break;

        case AST_Statement_Kind::INSERT: {

            for (s64 i = 0; i < stmt->insert.nodes_to_insert.count; i++) {

                AST_Node node = stmt->insert.nodes_to_insert[i];
                assert(node.kind == AST_Node_Kind::STATEMENT);

                ssa_emit_statement(builder, node.statement, scope);
            }
            break;
        }

        case AST_Statement_Kind::BLOCK: {
            Scope* block_scope = stmt->block.scope;
            for (s64 i = 0; i < stmt->block.statements.count; i++) {

                ssa_emit_statement(builder, stmt->block.statements[i], block_scope);
            }

            break;
        }

        case AST_Statement_Kind::ASSERT: {

            SSA_Register_Handle cond_reg = ssa_emit_expression(builder, stmt->assert_stmt.cond, scope);

            SSA_Register_Handle string_reg;

            if (stmt->assert_stmt.message) {
                SSA_Register_Handle str_ptr = ssa_emit_lvalue(builder, stmt->assert_stmt.message, scope);
                string_reg = str_ptr;
            } else {
                Type* string_pointer_type = pointer_type_get(builder->instance, builder->instance->type_string);
                string_reg = ssa_emit_load_immediate(builder, string_pointer_type, 0);
            }

            u32 op_offset = ssa_emit_op(builder, SSA_OP_ASSERT);
            ssa_emit_reg(builder, cond_reg);

            Source_Pos pos = source_pos(builder->instance, stmt);
            hash_table_add(&builder->program->instruction_origin_positions, { op_offset, (u32)builder->function_index, (u32)builder->block_index }, pos);


            ssa_emit_reg(builder, string_reg);
            break;
        }
    }
}

SSA_Register_Handle ssa_emit_lvalue(SSA_Builder* builder, AST_Expression* lvalue_expr, Scope* scope)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];

    switch (lvalue_expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            assert(lvalue_expr->identifier->decl);
            AST_Declaration* decl = lvalue_expr->identifier->decl;

            if (decl->kind == AST_Declaration_Kind::CONSTANT) {

                bool found = false;
                u32 index = 0;
                for (s64 i = 0; i < builder->program->constant_references.count; i++) {

                    SSA_Constant_Reference ref = builder->program->constant_references[i];

                    if (ref.ast_node.kind == AST_Node_Kind::DECLARATION &&
                        ref.ast_node.declaration == decl) {
                        found = true;
                        index = ref.const_index;
                        break;
                    }
                }

                assert(found);
                return ssa_emit_load_constant(builder, index);
            }
            assert(decl->kind == AST_Declaration_Kind::VARIABLE);

            bool is_struct = decl->resolved_type->kind == Type_Kind::STRUCT;
            bool is_param = decl->flags & AST_DECL_FLAG_PARAM;

            if (is_param) {
                assert(decl->flags & AST_DECL_FLAG_PARAMETER_STORAGE_REQUIRED || is_struct);
            }

            if (is_param && is_struct) {
                u32 param_index = decl->variable.index;
                if (function->sret) param_index++;
                return ssa_emit_load_param(builder, param_index);

            } else if (decl->flags & AST_DECL_FLAG_GLOBAL) {

                assert(decl->ident);
                u32 glob_index;
                bool found = ssa_find_global_variable(builder->program, decl->ident->atom, &glob_index);

                assert(found);

                return ssa_emit_global_pointer(builder, glob_index);

            } else {

                SSA_Register_Handle alloc_reg;
                bool found = ssa_find_alloc(builder, decl, &alloc_reg);
                assert(found);

                return alloc_reg;
            }
        }

        case AST_Expression_Kind::UNARY: assert(false); break;
        case AST_Expression_Kind::BINARY: assert(false); break;

        case AST_Expression_Kind::MEMBER: {
            AST_Declaration* field = lvalue_expr->member.member_name->decl;
            assert(field);

            assert(field->kind == AST_Declaration_Kind::STRUCT_MEMBER);

            auto index = field->variable.index;

            Type *base_type = lvalue_expr->member.base->resolved_type;
            Type *struct_type = nullptr;

            SSA_Register_Handle base_lvalue;

            if (base_type->kind == Type_Kind::STRUCT) {
                struct_type = base_type;
                base_lvalue = ssa_emit_lvalue(builder, lvalue_expr->member.base, scope);
            } else {
                assert(base_type->kind == Type_Kind::POINTER);
                assert(base_type->pointer.base->kind == Type_Kind::STRUCT);
                struct_type = base_type->pointer.base;
                base_lvalue = ssa_emit_expression(builder, lvalue_expr->member.base, scope);
            }

            assert(struct_type);
            return ssa_emit_struct_offset(builder, base_lvalue, struct_type, index);
        }

        case AST_Expression_Kind::IMPLICIT_MEMBER: assert(false); break;
        case AST_Expression_Kind::SUBSCRIPT: assert(false); break;

        case AST_Expression_Kind::CALL: {
            assert(lvalue_expr->resolved_type->kind == Type_Kind::STRUCT);

            return ssa_emit_expression(builder, lvalue_expr, scope);
        }

        case AST_Expression_Kind::ADDRESS_OF: assert(false); break;

        case AST_Expression_Kind::DEREF: {
            return ssa_emit_expression(builder, lvalue_expr->unary.operand, scope);
        }

        case AST_Expression_Kind::CAST: assert(false); break;

        case AST_Expression_Kind::COMPOUND: {

            if (lvalue_expr->flags & AST_EXPR_FLAG_CONST) {

                u32 constant_index = ssa_emit_constant(builder->instance, builder->program, lvalue_expr);
                return ssa_emit_load_constant(builder, constant_index);

            } else {

                SSA_Register_Handle compound_alloc_reg;
                bool found = ssa_find_alloc(builder, lvalue_expr, &compound_alloc_reg);
                assert(found);

                for (s64 i = 0; i < lvalue_expr->compound.expressions.count; i++) {

                    AST_Expression* expr = lvalue_expr->compound.expressions[i];
                    bool member_is_aggregate = expr->resolved_type->kind == Type_Kind::STRUCT;

                    SSA_Register_Handle value_reg;
                    if (member_is_aggregate) {
                        value_reg = ssa_emit_lvalue(builder, expr, scope);
                    } else {
                        value_reg = ssa_emit_expression(builder, expr, scope);
                    }

                    SSA_Register_Handle ptr_reg = ssa_emit_struct_offset(builder, compound_alloc_reg, lvalue_expr->resolved_type, i);

                    if (member_is_aggregate) {
                        ssa_emit_memcpy(builder, ptr_reg, value_reg, expr->resolved_type->bit_size);
                    } else {
                        ssa_emit_store_ptr(builder, expr->resolved_type->bit_size, ptr_reg, value_reg);
                    }
                }

                return compound_alloc_reg;
            }
        }

        case AST_Expression_Kind::TYPE: assert(false); break;

        case AST_Expression_Kind::RUN: {
            assert(lvalue_expr->run.generated_expression);
            assert(lvalue_expr->resolved_type->kind == Type_Kind::STRUCT);

            return ssa_emit_lvalue(builder, lvalue_expr->run.generated_expression, scope);
        }

        case AST_Expression_Kind::SIZEOF: assert(false); break;
        case AST_Expression_Kind::ALIGNOF: assert(false); break;
        case AST_Expression_Kind::OFFSETOF: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: assert(false); break;
        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

        case AST_Expression_Kind::STRING_LITERAL: {
            u32 string_data_const_index = ssa_emit_constant(builder->instance, builder->program, lvalue_expr);
            SSA_Register_Handle string_data_ptr = ssa_emit_load_constant(builder, string_data_const_index);
            return string_data_ptr;
        }
    }

    assert(false);
    return {};
}

SSA_Register_Handle ssa_emit_expression(SSA_Builder* builder, AST_Expression* expr, Scope* scope)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];

    SSA_Register_Handle result_reg = {};

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            assert(expr->identifier->decl);
            AST_Declaration* decl = expr->identifier->decl;

            if (decl->kind == AST_Declaration_Kind::VARIABLE) {

                if (decl->flags & AST_DECL_FLAG_PARAM) {

                    if (decl->flags & AST_DECL_FLAG_PARAMETER_STORAGE_REQUIRED) {

                        assert(decl->variable.index >= 0 && decl->variable.index < function->param_count);

                        SSA_Register_Handle alloc_reg;
                        bool found = ssa_find_alloc(builder, decl, &alloc_reg);
                        assert(found);

                        result_reg = ssa_emit_load_ptr(builder, decl->resolved_type, alloc_reg);

                    } else {

                        u32 param_index = decl->variable.index;
                        if (function->sret) param_index++;

                        result_reg = ssa_emit_load_param(builder, param_index);
                    }

                } else {

                    auto lvalue = ssa_emit_lvalue(builder, expr, scope);
                    if (expr->resolved_type->kind == Type_Kind::STRUCT) {
                        result_reg = lvalue;
                    } else {
                        result_reg = ssa_emit_load_ptr(builder, expr->resolved_type, lvalue);
                    }
                }

            } else {
                assert(decl->kind == AST_Declaration_Kind::CONSTANT);
                assert(!(decl->flags & AST_DECL_FLAG_PARAMETER_STORAGE_REQUIRED));

                result_reg = ssa_emit_load_constant_value(builder, expr, scope);
            }
            break;
        }

        case AST_Expression_Kind::UNARY: {

            switch (expr->unary.op) {
                default: assert(false); break;

                case '-': {
                    assert(expr->unary.operand->resolved_type->bit_size % 8 == 0);
                    auto size = expr->unary.operand->resolved_type->bit_size / 8;
                    assert(size >= 0 && size <= U8_MAX);

                    SSA_Register_Handle op_reg = ssa_emit_expression(builder, expr->unary.operand, scope);
                    SSA_Register_Handle im_zero_reg = ssa_emit_load_immediate(builder, expr->unary.operand->resolved_type, 0);

                    result_reg = ssa_register_create(builder, expr->resolved_type);
                    ssa_emit_op(builder, SSA_OP_SUB);
                    ssa_emit_8(builder, (u8)size);
                    ssa_emit_reg(builder, result_reg);
                    ssa_emit_reg(builder, im_zero_reg);
                    ssa_emit_reg(builder, op_reg);
                    break;
                }
            }
            break;
        }

        case AST_Expression_Kind::BINARY: {

            Type* left_type = expr->binary.lhs->resolved_type;
            Type* right_type = expr->binary.rhs->resolved_type;

            SSA_Register_Handle left_reg = ssa_emit_expression(builder, expr->binary.lhs, scope);
            SSA_Register_Handle right_reg = ssa_emit_expression(builder, expr->binary.rhs, scope);

            assert(expr->binary.lhs->resolved_type->bit_size % 8 == 0);
            auto size = expr->binary.lhs->resolved_type->bit_size / 8;
            assert(size >= 0 && size <= U8_MAX);

            if (left_type->kind != Type_Kind::POINTER || is_binary_cmp_op((Token_Kind)expr->binary.op)) {

                switch (expr->binary.op) {
                    case '+': ssa_emit_op(builder, SSA_OP_ADD); break;
                    case '-': ssa_emit_op(builder, SSA_OP_SUB); break;
                    case '*': ssa_emit_op(builder, SSA_OP_MUL); break;
                    case '/': ssa_emit_op(builder, SSA_OP_DIV); break;
                    case '<': ssa_emit_op(builder, SSA_OP_LT); break;
                    case '>': ssa_emit_op(builder, SSA_OP_GT); break;
                    case TOK_EQ: ssa_emit_op(builder, SSA_OP_EQ); break;
                    case TOK_NEQ: ssa_emit_op(builder, SSA_OP_NEQ); break;
                    case TOK_LTEQ: ssa_emit_op(builder, SSA_OP_LTEQ); break;
                    case TOK_GTEQ: ssa_emit_op(builder, SSA_OP_GTEQ); break;
                    default: assert(false);
                }

                result_reg = ssa_register_create(builder, expr->resolved_type);

                ssa_emit_8(builder, (u8)size);
                ssa_emit_reg(builder, result_reg);
                ssa_emit_reg(builder, left_reg);
                ssa_emit_reg(builder, right_reg);

            } else {

                assert(left_type->kind == Type_Kind::POINTER);

                if (right_type->kind == Type_Kind::INTEGER) {


                    if (expr->binary.op == '-') {

                        SSA_Register_Handle zero_reg = ssa_emit_load_immediate(builder, expr->binary.rhs->resolved_type, 0);
                        SSA_Register_Handle new_right = ssa_register_create(builder, expr->binary.rhs->resolved_type);

                        // TODO:  Negate op?
                        ssa_emit_op(builder, SSA_OP_SUB);
                        ssa_emit_8(builder, size);
                        ssa_emit_reg(builder, new_right);
                        ssa_emit_reg(builder, zero_reg);
                        ssa_emit_reg(builder, right_reg);

                        right_reg = new_right;
                    }


                    result_reg = ssa_emit_pointer_offset(builder, left_type, left_reg, right_reg);

                } else {
                    assert(expr->binary.op == '-');

                    result_reg = ssa_emit_pointer_diff(builder, left_type->pointer.base->bit_size, left_reg, right_reg);
                }
            }

            break;
        }

        case AST_Expression_Kind::MEMBER: {
            if (expr->resolved_type->kind == Type_Kind::ENUM) {
                AST_Declaration* mem_decl = expr->member.member_name->decl;
                assert(mem_decl);
                assert(mem_decl->kind == AST_Declaration_Kind::ENUM_MEMBER);

                assert(mem_decl->enum_member.value_expr->kind == AST_Expression_Kind::INTEGER_LITERAL ||
                       mem_decl->enum_member.value_expr->kind == AST_Expression_Kind::IDENTIFIER);
                assert(mem_decl->enum_member.value_expr->resolved_type == expr->resolved_type->enumeration.strict_type);

                result_reg = ssa_emit_load_enum(builder, expr->resolved_type, mem_decl->enum_member.index_in_type);

            } else {
                auto lvalue = ssa_emit_lvalue(builder, expr, scope);
                result_reg = ssa_emit_load_ptr(builder, expr->resolved_type, lvalue);
            }
            break;
        }

        case AST_Expression_Kind::IMPLICIT_MEMBER: {

            AST_Declaration* mem_decl = expr->implicit_member.member_name->decl;

            assert(mem_decl);
            assert(mem_decl->kind == AST_Declaration_Kind::ENUM_MEMBER);

            assert(mem_decl->enum_member.value_expr->kind == AST_Expression_Kind::INTEGER_LITERAL);
            assert(mem_decl->enum_member.value_expr->resolved_type == expr->resolved_type->enumeration.strict_type);

            result_reg = ssa_emit_load_enum(builder, expr->resolved_type, mem_decl->enum_member.index_in_type);
            break;
        }

        case AST_Expression_Kind::SUBSCRIPT: assert(false); break;

        case AST_Expression_Kind::CALL: {

            // Only support calling via identifier for now...
            assert(expr->call.base->kind == AST_Expression_Kind::IDENTIFIER);
            Atom name = expr->call.base->identifier->atom;

            u32 fn_index;
            bool found = ssa_find_function(builder->program, name, &fn_index);
            assert(found);

            SSA_Function *callee = &builder->program->functions[fn_index];

            SSA_Register_Handle sret_reg;
            if (callee->sret) {
                bool found = ssa_find_alloc(builder, expr, &sret_reg);
                assert(found);

                ssa_emit_op(builder, SSA_OP_PUSH);
                ssa_emit_reg(builder, sret_reg);
            }

            for (s64 i = 0; i < expr->call.args.count; i++) {
                AST_Expression* arg_expr = expr->call.args[i];
                SSA_Register_Handle arg_reg;
                if (arg_expr->resolved_type->kind == Type_Kind::STRUCT) {

                    bool found = ssa_find_alloc(builder, arg_expr, &arg_reg);
                    assert(found);

                    SSA_Register_Handle src_ptr_reg = ssa_emit_lvalue(builder, arg_expr, scope);
                    ssa_emit_memcpy(builder, arg_reg, src_ptr_reg, arg_expr->resolved_type->bit_size);

                } else {

                    arg_reg = ssa_emit_expression(builder, arg_expr, scope);
                }

                ssa_emit_op(builder, SSA_OP_PUSH);
                ssa_emit_reg(builder, arg_reg);
            }

            if (callee->foreign) {
                ssa_emit_op(builder, SSA_OP_CALL_FOREIGN);
            } else {
                ssa_emit_op(builder, SSA_OP_CALL);
            }

            result_reg = ssa_register_create(builder, expr->resolved_type);
            ssa_emit_32(builder, result_reg.index); // Don't use ssa_emit_reg because that'll mark this register as used

            ssa_emit_32(builder, fn_index);

            u32 arg_pop_count = expr->call.args.count;
            if (callee->sret) arg_pop_count++;

            if (callee->foreign) {
                assert(!callee->sret);
                assert(arg_pop_count <= U16_MAX);
                ssa_emit_16(builder, arg_pop_count);
            }

            if (arg_pop_count) {
                ssa_emit_op(builder, SSA_OP_POP_N);
                ssa_emit_32(builder, arg_pop_count);
            }

            if (callee->sret) {
                result_reg = sret_reg;
            }
            break;
        }

        case AST_Expression_Kind::ADDRESS_OF: {
            return ssa_emit_lvalue(builder, expr->unary.operand, scope);
            break;
        }

        case AST_Expression_Kind::DEREF: {
            Type *operand_type = expr->unary.operand->resolved_type;

            if (operand_type->kind == Type_Kind::STRUCT) {
                assert(false);
            } else {
                SSA_Register_Handle ptr_reg = ssa_emit_expression(builder, expr->unary.operand, scope);
                return ssa_emit_load_ptr(builder, expr->resolved_type, ptr_reg);
            }

            assert(false);
            break;
        }

        case AST_Expression_Kind::CAST: {

            Type* to_type = expr->cast.ts->resolved_type;
            Type* from_type = expr->cast.operand->resolved_type;

            SSA_Register_Handle operand_reg = ssa_emit_expression(builder, expr->cast.operand, scope);
            result_reg = ssa_emit_cast(builder, from_type, to_type, operand_reg);
            break;
        }

        case AST_Expression_Kind::COMPOUND: {
            result_reg = ssa_emit_lvalue(builder, expr, scope);
            break;
        }

        case AST_Expression_Kind::TYPE: assert(false); break;

        case AST_Expression_Kind::RUN: {

            assert(expr->run.generated_expression);

            result_reg = ssa_emit_expression(builder, expr->run.generated_expression, scope);

            break;
        }

        case AST_Expression_Kind::SIZEOF: {
            s64 size = expr->sizeof_expr.operand->resolved_type->bit_size;
            assert(size % 8 == 0);
            size /= 8;
            result_reg = ssa_emit_load_immediate(builder, expr->resolved_type, size);
            break;
        }

        case AST_Expression_Kind::ALIGNOF: {
            s64 alignment = expr->sizeof_expr.operand->resolved_type->alignment;
            result_reg = ssa_emit_load_immediate(builder, expr->resolved_type, alignment);
            break;
        }

        case AST_Expression_Kind::OFFSETOF: {
            AST_Declaration* agg_decl = expr->offsetof_expr.struct_ident->decl;
            assert(agg_decl->kind == AST_Declaration_Kind::STRUCT);

            Type* agg_type = agg_decl->resolved_type;
            assert(agg_type->kind == Type_Kind::STRUCT);

            AST_Declaration* mem_decl = expr->offsetof_expr.member_ident->decl;
            assert(mem_decl->kind == AST_Declaration_Kind::STRUCT_MEMBER);

            s64 member_index = mem_decl->variable.index;

            assert(member_index >= 0 && member_index < agg_type->structure.members.count);

            s64 offset = agg_type->structure.members[member_index].offset;
            assert(offset % 8 == 0);
            result_reg = ssa_emit_load_immediate(builder, expr->resolved_type, offset / 8);
            break;
        }

        case AST_Expression_Kind::INTEGER_LITERAL: {
            result_reg = ssa_emit_load_immediate(builder, expr->resolved_type, expr->integer_literal);
            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;

        case AST_Expression_Kind::CHAR_LITERAL: {
            result_reg = ssa_emit_load_immediate(builder, expr->resolved_type, expr->integer_literal);
            break;
        }

        case AST_Expression_Kind::BOOL_LITERAL: {
            result_reg = ssa_emit_load_immediate(builder, expr->resolved_type, expr->bool_literal);
            break;
        }

        case AST_Expression_Kind::NULL_LITERAL: {
            result_reg = ssa_emit_load_immediate(builder, expr->resolved_type, 0);
            break;
        }

        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
    }

    return result_reg;
}

SSA_Register_Handle ssa_emit_global_pointer(SSA_Builder *builder, u32 global_index)
{
    assert(global_index >= 0 && global_index < builder->program->globals.count);

    Type* global_type = builder->program->globals[global_index].type;
    Type* pointer_type = pointer_type_get(builder->instance, global_type);

    SSA_Register_Handle result = ssa_register_create(builder, pointer_type);
    ssa_emit_op(builder, SSA_OP_GLOB_PTR);
    ssa_emit_reg(builder, result);
    ssa_emit_32(builder, global_index);

    return result;
}

SSA_Register_Handle ssa_emit_bitcast(SSA_Builder* builder, Type* from_type, Type* to_type, SSA_Register_Handle operand_reg)
{
    assert(from_type->bit_size == to_type->bit_size);
    assert(to_type->bit_size <= 64);

    SSA_Register_Handle result_reg = ssa_register_create(builder, to_type);

    ssa_emit_op(builder, SSA_OP_BITCAST);
    ssa_emit_reg(builder, result_reg);
    ssa_emit_reg(builder, operand_reg);

    return result_reg;
}

SSA_Register_Handle ssa_emit_trunc(SSA_Builder* builder, Type* target_type, SSA_Register_Handle operand_reg)
{
    assert(target_type->kind == Type_Kind::INTEGER);
    auto target_bit_size = target_type->bit_size;

    assert(target_bit_size % 8 == 0);
    auto target_byte_size = target_bit_size / 8;
    assert(target_byte_size >= 0 && target_byte_size < U8_MAX);

    SSA_Register_Handle result_reg = ssa_register_create(builder, target_type);
    ssa_emit_op(builder, SSA_OP_TRUNC);
    ssa_emit_8(builder, target_byte_size);
    ssa_emit_reg(builder, result_reg);
    ssa_emit_reg(builder, operand_reg);

    return result_reg;
}

SSA_Register_Handle ssa_emit_sext(SSA_Builder* builder, Type* target_type, s64 source_bit_size, SSA_Register_Handle operand_reg)
{
    assert(target_type->kind == Type_Kind::INTEGER);
    auto target_bit_size = target_type->bit_size;

    assert(target_bit_size % 8 == 0);
    auto target_byte_size = target_bit_size / 8;
    assert(target_byte_size >= 0 && target_byte_size < U8_MAX);

    assert(source_bit_size % 8 == 0);
    auto source_byte_size = source_bit_size / 8;
    assert(source_byte_size >= 0 && source_byte_size < U8_MAX);

    SSA_Register_Handle result_reg = ssa_register_create(builder, target_type);
    ssa_emit_op(builder, SSA_OP_SEXT);
    ssa_emit_8(builder, target_byte_size);
    ssa_emit_8(builder, source_byte_size);
    ssa_emit_reg(builder, result_reg);
    ssa_emit_reg(builder, operand_reg);

    return result_reg;
}

SSA_Register_Handle ssa_emit_zext(SSA_Builder* builder, Type* target_type, SSA_Register_Handle operand_reg)
{
    assert(target_type->kind == Type_Kind::INTEGER);
    auto target_bit_size = target_type->bit_size;

    assert(target_bit_size % 8 == 0);
    auto target_byte_size = target_bit_size / 8;
    assert(target_byte_size >= 0 && target_byte_size < U8_MAX);

    SSA_Register_Handle result_reg = ssa_register_create(builder, target_type);
    ssa_emit_op(builder, SSA_OP_ZEXT);
    ssa_emit_8(builder, target_byte_size);
    ssa_emit_reg(builder, result_reg);
    ssa_emit_reg(builder, operand_reg);

    return result_reg;
}

SSA_Register_Handle ssa_emit_alloc(SSA_Builder* builder, Type* type)
{
    assert(type->bit_size >= 0);
    assert(type->bit_size % 8 == 0);
    s64 byte_size = type->bit_size / 8;

    SSA_Register_Handle alloc_reg = ssa_register_create(builder, type, true);
    ssa_emit_op(builder, SSA_OP_ALLOC);
    ssa_emit_reg(builder, alloc_reg);
    ssa_emit_64(builder, byte_size);

    builder->program->functions[builder->function_index].total_alloc_size += byte_size;

    return { alloc_reg };
}

void ssa_emit_memcpy(SSA_Builder* builder, SSA_Register_Handle dest_ptr_reg, SSA_Register_Handle src_ptr_reg, s64 bit_size)
{
    assert(bit_size >= 0);
    assert(bit_size % 8 == 0);
    s64 size = bit_size / 8;

    if (dest_ptr_reg.index == src_ptr_reg.index) return;

    ssa_emit_op(builder, SSA_OP_MEMCPY);
    ssa_emit_reg(builder, dest_ptr_reg);
    ssa_emit_reg(builder, src_ptr_reg);
    ssa_emit_64(builder, size);
}

void ssa_emit_store_ptr(SSA_Builder* builder, s64 bit_size, SSA_Register_Handle dest_reg, SSA_Register_Handle source_reg)
{
    assert(bit_size % 8 == 0);
    auto size = bit_size / 8;
    assert(size >= 0 && size <= U8_MAX);
    assert(size <= 8);

    ssa_emit_op(builder, SSA_OP_STORE_PTR);
    ssa_emit_8(builder, size);
    ssa_emit_reg(builder, dest_reg);
    ssa_emit_reg(builder, source_reg);
}

SSA_Register_Handle ssa_emit_load_immediate(SSA_Builder* builder, Type* type, u64 immediate_value)
{
    assert(type->kind == Type_Kind::INTEGER || type->kind == Type_Kind::BOOLEAN || type->kind == Type_Kind::POINTER || type->kind == Type_Kind::ENUM);

    assert(type->bit_size % 8 == 0);
    auto size = type->bit_size / 8;
    assert(size >= 0 && size < U8_MAX);

    ssa_emit_op(builder, SSA_OP_LOAD_IM);
    ssa_emit_8(builder, size);
    SSA_Register_Handle result = ssa_register_create(builder, type);
    ssa_emit_reg(builder, result);

    switch (size) {
        default: assert(false); break;
        case 1: ssa_emit_8(builder, (u8)immediate_value); break;
        case 2: ssa_emit_16(builder, (u16)immediate_value); break;
        case 4: ssa_emit_32(builder, (u32)immediate_value); break;
        case 8: ssa_emit_64(builder, (u64)immediate_value); break;
    }

    return result;
}

SSA_Register_Handle ssa_emit_load_param(SSA_Builder* builder, u32 param_index)
{
    SSA_Function* func = &builder->program->functions[builder->function_index];
    Type* param_type = nullptr;

    if (func->sret) {
        if (param_index == 0) {
            param_type = func->type->function.return_type;
        } else {
            param_type = func->type->function.param_types[param_index - 1];
        }
    } else {
        param_type = func->type->function.param_types[param_index];
    }

    SSA_Register_Handle result = ssa_register_create(builder, param_type);
    ssa_emit_op(builder, SSA_OP_LOAD_PARAM);
    ssa_emit_reg(builder, result);
    ssa_emit_32(builder, param_index);

    return result;
}

SSA_Register_Handle ssa_emit_load_ptr(SSA_Builder* builder, Type* type, SSA_Register_Handle ptr_reg)
{
    assert(type->bit_size % 8 == 0);
    auto size = type->bit_size / 8;
    assert(size > 0 && size < U8_MAX);
    assert(size <= 8);

    SSA_Register_Handle dest_reg = ssa_register_create(builder, type);

    ssa_emit_op(builder, SSA_OP_LOAD_PTR);
    ssa_emit_8(builder, size);
    ssa_emit_reg(builder, dest_reg);
    ssa_emit_reg(builder, ptr_reg);

    return dest_reg;
}

SSA_Register_Handle ssa_emit_load_constant(SSA_Builder *builder, u32 index)
{
    Type* type = builder->program->constants[index].type;
    SSA_Register_Handle dest_reg = ssa_register_create(builder, type);

    assert(index < builder->program->constants.count);

    ssa_emit_op(builder, SSA_OP_LOAD_CONST);
    ssa_emit_reg(builder, dest_reg);
    ssa_emit_32(builder, builder->program->constants[index].offset);

    return dest_reg;
}

SSA_Register_Handle ssa_emit_load_enum(SSA_Builder* builder, Type* enum_type, s64 index)
{
    assert(enum_type->kind == Type_Kind::ENUM);
    assert(enum_type->enumeration.members.count > index);

    assert(enum_type->bit_size % 8 == 0);

    SSA_Register_Handle result = ssa_register_create(builder, enum_type);

    ssa_emit_op(builder, SSA_OP_LOAD_ENUM);
    ssa_emit_reg(builder, result);
    ssa_emit_64(builder, index);

    switch (enum_type->bit_size) {
        default: assert(false && "Invalid enum size in ssa_emit_load_enum"); break;

        case 8: ssa_emit_8(builder, enum_type->enumeration.members[index].value); break;
        case 16: ssa_emit_16(builder, enum_type->enumeration.members[index].value); break;
        case 32: ssa_emit_32(builder, enum_type->enumeration.members[index].value); break;
        case 64: ssa_emit_64(builder, enum_type->enumeration.members[index].value); break;
    }

    return result;
}

SSA_Register_Handle ssa_emit_struct_offset(SSA_Builder* builder, SSA_Register_Handle struct_ptr_reg, Type* struct_type, s64 index)
{
    assert(struct_type->kind == Type_Kind::STRUCT);
    assert(index <= struct_type->structure.members.count);
    auto bit_offset = struct_type->structure.members[index].offset;

    assert(bit_offset % 8 == 0);
    auto offset = bit_offset / 8;
    assert(offset >= 0 && offset <= U32_MAX);
    assert(index >= 0 && index <= U16_MAX);


    Type* member_type = struct_type->structure.members[index].type;
    Type* result_type = pointer_type_get(builder->instance, member_type);

    ssa_emit_op(builder, SSA_OP_STRUCT_OFFSET);
    SSA_Register_Handle result = ssa_register_create(builder, result_type);
    ssa_emit_reg(builder, result);
    ssa_emit_reg(builder, struct_ptr_reg);
    ssa_emit_32(builder, (u32)offset);
    ssa_emit_16(builder, (u16)index);

    return result;
}

SSA_Register_Handle ssa_emit_pointer_offset(SSA_Builder* builder, Type* pointer_type, SSA_Register_Handle base_reg, SSA_Register_Handle index_reg)
{
    assert(pointer_type->kind == Type_Kind::POINTER);
    auto pointee_bit_size = pointer_type->pointer.base->bit_size;

    assert(pointee_bit_size % 8 == 0);
    s64 pointee_size = pointee_bit_size / 8;

    SSA_Register_Handle result = ssa_register_create(builder, pointer_type);

    ssa_emit_op(builder, SSA_OP_POINTER_OFFSET);
    ssa_emit_64(builder, pointee_size);
    ssa_emit_reg(builder, result);
    ssa_emit_reg(builder, base_reg);
    ssa_emit_reg(builder, index_reg);

    return result;
}

SSA_Register_Handle ssa_emit_pointer_diff(SSA_Builder* builder, s64 pointee_bit_size, SSA_Register_Handle left_reg, SSA_Register_Handle right_reg)
{
    assert(pointee_bit_size % 8 == 0);
    s64 pointee_size = pointee_bit_size / 8;

    SSA_Register_Handle result = ssa_register_create(builder, builder->instance->builtin_type_int);

    ssa_emit_op(builder, SSA_OP_POINTER_DIFF);
    ssa_emit_64(builder, pointee_size);
    ssa_emit_reg(builder, result);
    ssa_emit_reg(builder, left_reg);
    ssa_emit_reg(builder, right_reg);

    return result;
}

void ssa_emit_jmp_if(SSA_Builder* builder, SSA_Register_Handle cond_reg, u32 true_block, u32 false_block)
{
    assert(!ssa_block_exits(builder, builder->block_index));

    ssa_emit_op(builder, SSA_OP_JMP_IF);
    ssa_emit_reg(builder, cond_reg);
    ssa_emit_32(builder, true_block);
    ssa_emit_32(builder, false_block);

    SSA_Function* function = &builder->program->functions[builder->function_index];
    darray_append(&function->blocks[true_block].incoming, (u32)builder->block_index);
    darray_append(&function->blocks[false_block].incoming, (u32)builder->block_index);
}

void ssa_emit_jmp(SSA_Builder* builder, u32 block)
{
    ssa_emit_op(builder, SSA_OP_JMP);
    ssa_emit_32(builder, block);

    SSA_Function* function = &builder->program->functions[builder->function_index];
    darray_append(&function->blocks[block].incoming, (u32)builder->block_index);
}

SSA_Register_Handle ssa_emit_cast(SSA_Builder* builder, Type* from_type, Type* to_type, SSA_Register_Handle operand_reg)
{
    switch (from_type->kind) {

        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;

        case Type_Kind::INTEGER: {
            switch (to_type->kind) {
                case Type_Kind::INVALID: assert(false); break;
                case Type_Kind::VOID: assert(false); break;

                case Type_Kind::INTEGER: {
                    return ssa_emit_integer_integer_cast(builder, from_type, to_type, operand_reg);
                }

                case Type_Kind::BOOLEAN: assert(false); break;

                case Type_Kind::POINTER: {
                    return ssa_emit_bitcast(builder, from_type, to_type, operand_reg);
                    break;
                }

                case Type_Kind::ARRAY: assert(false); break;
                case Type_Kind::FUNCTION: assert(false); break;
                case Type_Kind::STRUCT: assert(false); break;
                case Type_Kind::ENUM: assert(false); break;
            }
        }

        case Type_Kind::BOOLEAN: assert(false); break;

        case Type_Kind::POINTER: {
            switch (to_type->kind) {
                default: assert(false); break;

                case Type_Kind::POINTER:
                case Type_Kind::INTEGER: {
                    assert(to_type->bit_size == 64);
                    return ssa_emit_bitcast(builder, from_type, to_type, operand_reg);
                }
            }
        }

        case Type_Kind::ARRAY: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;
        case Type_Kind::STRUCT: assert(false); break;
        case Type_Kind::ENUM: assert(false); break;
    }

    assert(false);
    return {};
}

SSA_Register_Handle ssa_emit_integer_integer_cast(SSA_Builder* builder, Type* from_type, Type* to_type, SSA_Register_Handle operand_reg)
{
    assert(from_type->kind == Type_Kind::INTEGER);
    assert(to_type->kind == Type_Kind::INTEGER);

    if (from_type->bit_size == to_type->bit_size) {
        // bitcast
        return operand_reg;

    } else if (from_type->bit_size > to_type->bit_size) {
        return ssa_emit_trunc(builder, to_type, operand_reg);

    } else {
        if (from_type->integer.sign) {
            return ssa_emit_sext(builder, to_type, from_type->bit_size, operand_reg);
        } else {
            return ssa_emit_zext(builder, to_type, operand_reg);
        }
    }
}

u32 ssa_emit_op(SSA_Builder* builder, SSA_Op op)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];
    SSA_Block* block = &function->blocks[builder->block_index];

    assert(!block->exits);

    if (op == SSA_OP_RET ||
        op == SSA_OP_JMP ||
        op == SSA_OP_JMP_IF) {

        assert(!block->exits);

        block->exits = true;

    }

    u32 result = block->bytes.count;
    darray_append(&block->bytes, (u8)op);
    return result;
}

void ssa_emit_reg(SSA_Builder* builder, SSA_Register_Handle reg_handle)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];

    u32 index = reg_handle.index;
    assert(index < function->registers.count);
    function->registers[index].used = true;

    ssa_emit_32(builder, index);
}

void ssa_emit_8(SSA_Builder* builder, u8 value)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];
    ssa_emit_8(&function->blocks[builder->block_index].bytes, value);
}

void ssa_emit_16(SSA_Builder* builder, u16 value)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];
    ssa_emit_16(&function->blocks[builder->block_index].bytes, value);
}

void ssa_emit_32(SSA_Builder* builder, u32 value)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];
    ssa_emit_32(&function->blocks[builder->block_index].bytes, value);
}

void ssa_emit_64(SSA_Builder* builder, u64 value)
{
    SSA_Function* function = &builder->program->functions[builder->function_index];
    ssa_emit_64(&function->blocks[builder->block_index].bytes, value);
}

void ssa_emit_8(DArray<u8> *bytes, u8 value)
{
    darray_append(bytes, value);
}

void ssa_emit_16(DArray<u8> *bytes, u16 value)
{
    // Little endian
    darray_append(bytes, (u8)(value & 0xFF));
    darray_append(bytes, (u8)((value >> 8) & 0xFF));
}

void ssa_emit_32(DArray<u8> *bytes, u32 value)
{
    // Little endian
    darray_append(bytes, (u8)(value & 0xFF));
    darray_append(bytes, (u8)((value >> 8) & 0xFF));
    darray_append(bytes, (u8)((value >> 16) & 0xFF));
    darray_append(bytes, (u8)((value >> 24) & 0xFF));
}

void ssa_emit_64(DArray<u8> *bytes, u64 value)
{
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

SSA_Register_Handle ssa_emit_load_constant_value(SSA_Builder* builder, AST_Expression* expr, Scope* scope)
{
    switch (expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            AST_Declaration* decl = expr->identifier->decl;
            assert(decl->kind == AST_Declaration_Kind::CONSTANT);

            AST_Expression* value_expr = decl->constant.value;
            assert(value_expr->resolved_type->kind == Type_Kind::INTEGER);

            return ssa_emit_load_immediate(builder, expr->resolved_type, value_expr->integer_literal);
            break;
        }

        case AST_Expression_Kind::UNARY: assert(false); break;
        case AST_Expression_Kind::BINARY: assert(false); break;
        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::IMPLICIT_MEMBER: assert(false); break;
        case AST_Expression_Kind::SUBSCRIPT: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::ADDRESS_OF: assert(false); break;
        case AST_Expression_Kind::DEREF: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;
        case AST_Expression_Kind::COMPOUND: assert(false); break;
        case AST_Expression_Kind::RUN: assert(false); break;
        case AST_Expression_Kind::SIZEOF: assert(false); break;
        case AST_Expression_Kind::ALIGNOF: assert(false); break;
        case AST_Expression_Kind::OFFSETOF: assert(false); break;
        case AST_Expression_Kind::TYPE: assert(false); break;
        case AST_Expression_Kind::INTEGER_LITERAL: assert(false); break;
        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
        }

    assert(false);
    return {};
}

u32 ssa_emit_constant(Instance* inst, SSA_Program* program, AST_Expression* const_expr, DArray<u8>* bytes/*=nullptr*/)
{
    assert(const_expr->flags & AST_EXPR_FLAG_CONST);

    s64 byte_size = const_expr->resolved_type->bit_size / 8;

    Temp_Arena tarena = temp_arena(nullptr);
    Allocator ta = arena_allocator_create(tarena.arena);

    bool own_bytes = false;
    DArray<u8> temp_bytes;
    if (!bytes) {
        own_bytes = true;

        temp_bytes = darray_create<u8>(&ta, byte_size);
        bytes = &temp_bytes;
    }

    s64 old_byte_count = bytes->count;

    s64 patch_offset = -1; // This might need to be a temp array later

    switch (const_expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;
        case AST_Expression_Kind::IDENTIFIER: assert(false); break;
        case AST_Expression_Kind::UNARY: assert(false); break;
        case AST_Expression_Kind::BINARY: assert(false); break;
        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::IMPLICIT_MEMBER: assert(false); break;
        case AST_Expression_Kind::SUBSCRIPT: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;

        case AST_Expression_Kind::ADDRESS_OF: assert(false); break;
        case AST_Expression_Kind::DEREF: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;

        case AST_Expression_Kind::COMPOUND: {
            assert(const_expr->resolved_type->kind == Type_Kind::STRUCT || const_expr->resolved_type->kind == Type_Kind::ARRAY);

            for (s64 i = 0; i < const_expr->compound.expressions.count; i++) {
                ssa_emit_constant(inst, program, const_expr->compound.expressions[i], bytes);
            }
            break;
        }

        case AST_Expression_Kind::TYPE: assert(false); break;

        case AST_Expression_Kind::RUN: assert(false); break;

        case AST_Expression_Kind::SIZEOF: assert(false); break;
        case AST_Expression_Kind::ALIGNOF: assert(false); break;
        case AST_Expression_Kind::OFFSETOF: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: {
            Type *inttype = const_expr->resolved_type;
            assert(inttype->kind == Type_Kind::INTEGER);

            switch (inttype->bit_size) {
                default: assert(false); break;
                case 8: ssa_emit_8(bytes, (u8)const_expr->integer_literal); break;
                case 16: ssa_emit_16(bytes, (u16)const_expr->integer_literal); break;
                case 32: ssa_emit_32(bytes, (u32)const_expr->integer_literal); break;
                case 64: ssa_emit_64(bytes, (u64)const_expr->integer_literal); break;
            }

            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

        case AST_Expression_Kind::STRING_LITERAL: {

            String str = atom_string(const_expr->string_literal);

            NSTRING_ASSERT_ZERO_TERMINATION(str);
            u32 str_index = ssa_emit_constant(program, Array_Ref((u8*)str.data, str.length + 1), inst->builtin_type_cstring, nullptr);

            s64 padding = 8 - (program->constant_memory.count % 8);
            if (padding % 8 != 0) {
                for (s64 i = 0; i < padding; i++) darray_append(&program->constant_memory, (u8)0);
            }
            patch_offset = program->constant_memory.count;

            assert(own_bytes); // We can't emit patch offset properly otherwise...
            ssa_emit_64(bytes, program->constants[str_index].offset);
            ssa_emit_64(bytes, str.length);

            break;
        }
    }

    assert(bytes->count - old_byte_count == byte_size);

    u32 result = 0;

    if (own_bytes) {

        s64 old_count = program->constants.count;
        result = ssa_emit_constant(program, temp_bytes, const_expr->resolved_type, const_expr);
        if (result >= old_count) {
            // Means this was the first occurance and actually emitted

            if (patch_offset >= 0) {
                darray_append(&program->constant_patch_offsets, patch_offset);
            }
        }

        temp_arena_release(tarena);
    }

    return result;
}

u32 ssa_emit_constant(SSA_Program* program, Array_Ref<u8> bytes, Type* type, AST_Expression* const_expr)
{
    u32 result = 0;

    if (const_expr) assert(type == const_expr->resolved_type);

    bool match = false;
    for (s64 i = 0; i < program->constants.count; i++) {
        SSA_Constant constant = program->constants[i];

        if (constant.type == type) {
            if (memcmp(&program->constant_memory[constant.offset], bytes.data, bytes.count) == 0) {
                match = true;
                result = i;
                break;
            }
        }
    }

    if (!match) {

        s64 padding = 8 - (program->constant_memory.count % 8);
        if (padding % 8 != 0) {
            for (s64 i = 0; i < padding; i++) darray_append(&program->constant_memory, (u8)0);
        }

        u32 offset = program->constant_memory.count;
        result = program->constants.count;

        darray_append(&program->constants, { type, offset, const_expr });

        darray_append_array(&program->constant_memory, Array_Ref<u8>(bytes));
    }

    return result;
}

Atom ssa_unique_function_name(Instance* inst, SSA_Program* program, String_Ref name)
{
    String_Ref original_name = name;

    bool unique = true;
    u64 counter = 1;

    Temp_Arena tarena = temp_arena(nullptr);
    Allocator ta = arena_allocator_create(tarena.arena);

    do {
        for (s64 i = 0; i < program->functions.count; i++) {
            if (string_equal(name, atom_string(program->functions[i].name))) {
                unique = false;
                break;
            }
        }

        if (!unique) {
            name = string_format(&ta, "%.*s.%llu", (int)original_name.length, original_name.data, counter++);
        }

    } while (!unique);

    Atom result = atom_get(name);

    temp_arena_release(tarena);

    return result;
}

String ssa_to_string(Instance* inst, Allocator* allocator, SSA_Program* program)
{
    String_Builder sb;
    string_builder_init(&sb, allocator);

    ssa_print(inst, &sb, program);

    String result = string_builder_to_string(&sb);

    string_builder_free(&sb);

    return result;
}

void ssa_print(Instance* inst, String_Builder* sb, SSA_Program* program)
{
    if (program->constant_memory.count) {
        bool newline = true;
        for (s64 i = 0; i < program->constant_memory.count; i++) {

            if (newline) string_builder_append(sb, "0x%.8x: ", i);
            newline = (i + 1) % 8 == 0;
            bool extra_space = (i + 1) % 4 == 0;

            string_builder_append(sb, "%.2x %s", program->constant_memory[i], extra_space ? " " : "");

            if (newline) string_builder_append(sb, "\n");
        }

        string_builder_append(sb, "\n");
    }

    if (program->globals.count) {
        for (s64 i = 0; i < program->globals.count; i++) {
            string_builder_append(sb, "@%u = ", i);
            ssa_print_constant(inst, sb, program, program->globals[i].initializer_constant_index);
            string_builder_append(sb, " <%s>\n", temp_type_string(inst, program->globals[i].type));
        }

        string_builder_append(sb, "\n");
    }

    for (s64 fi = 0; fi < program->functions.count; fi++) {
        if (fi != 0) string_builder_append(sb, "\n");

        SSA_Function* fn = &program->functions[fi];

        String name = atom_string(fn->name);
        if (fn->foreign) {
            string_builder_append(sb, "#foreign %s;\n", name.data);
            continue;
        }

        string_builder_append(sb, "%s:\n", name.data);


        s64 block_index = 0;
        int printed_block_count = 0;
        while (block_index >= 0 && block_index < fn->blocks.count) {
            printed_block_count++;
            SSA_Block* block = &fn->blocks[block_index];
            if (block->bytes.count > 0) {

                string_builder_append(sb, " %s:\n", atom_string(block->name).data);

                s64 ip = 0;

                while (ip < block->bytes.count) {
                    ip = ssa_print_instruction(inst, sb, program, fn, ip, block->bytes);
                }

            }

            block_index = block->next_index;
        }

        assert(printed_block_count == fn->blocks.count);
    }
}

void ssa_print_constant(Instance* inst, String_Builder* sb, SSA_Program* program, u32 index)
{
    assert(index < program->constants.count);
    SSA_Constant* constant = &program->constants[index];

    u8* ptr = program->constant_memory.data + constant->offset;
    ssa_print_pointer_value(sb, constant->type, ptr);
}

void ssa_print_pointer_value(String_Builder* sb, Type* type, u8* ptr)
{
    assert(type->bit_size % 8 == 0);
    auto byte_size = type->bit_size / 8;

    switch (type->kind) {

        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;

        case Type_Kind::INTEGER: {

            if (type->integer.sign) {
                switch (byte_size) {
                    default: assert(false); break;
                    case 1: string_builder_append(sb, "%hhd", *(s8*)ptr); break;
                    case 2: string_builder_append(sb, "%hd", *(s16*)ptr); break;
                    case 4: string_builder_append(sb, "%d", *(s32*)ptr); break;
                    case 8: string_builder_append(sb, "%lld", *(s64*)ptr); break;
                }
            } else {
                switch (byte_size) {
                    default: assert(false); break;
                    case 1: string_builder_append(sb, "%hhu", *(u8*)ptr); break;
                    case 2: string_builder_append(sb, "%hu", *(u16*)ptr); break;
                    case 4: string_builder_append(sb, "%u", *(u32*)ptr); break;
                    case 8: string_builder_append(sb, "%llu", *(u64*)ptr); break;
                }
            }

            break;
        }

        case Type_Kind::BOOLEAN: assert(false); break;
        case Type_Kind::POINTER: assert(false); break;
        case Type_Kind::ARRAY: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;

        case Type_Kind::STRUCT: {
            string_builder_append(sb, "{ ");
            for (s64 i = 0; i < type->structure.members.count; i++) {
                if (i > 0) string_builder_append(sb, ", ");
                ssa_print_pointer_value(sb, type->structure.members[i].type, ptr + type->structure.members[i].offset / 8);
            }
            string_builder_append(sb, " }");
            break;
        }

        case Type_Kind::ENUM: assert(false); break;
     }
}

s64 ssa_print_instruction(Instance* inst, String_Builder* sb, SSA_Program* program, SSA_Function* fn, s64 ip, Array_Ref<u8> bytes)
{
    assert(bytes.count);

    SSA_Op op = (SSA_Op)bytes[ip++];

    switch (op) {

        default: assert(false && "Invalid or unhandled instruction in ssa_print_instruction");

        case SSA_OP_NOP: assert(false); break;

#define BINOP_CASE(op) case SSA_OP_##op: { \
    u8 size_reg = *(u8*)&bytes[ip]; \
    ip += sizeof(u8); \
    u32 dest_reg = *(u32*)&bytes[ip]; \
    ip += sizeof(u32); \
    u32 left = *(u32*)&bytes[ip]; \
    ip += sizeof(u32); \
    u32 right = *(u32*)&bytes[ip]; \
    ip += sizeof(u32); \
    string_builder_append(sb, "  %%%u = "#op" %hhu %%%u %%%u\n", dest_reg, size_reg, left, right); \
    break; \
}

        BINOP_CASE(ADD);
        BINOP_CASE(SUB);
        BINOP_CASE(MUL);
        BINOP_CASE(DIV);
        BINOP_CASE(LT);
        BINOP_CASE(GT);
        BINOP_CASE(EQ);
        BINOP_CASE(NEQ);
        BINOP_CASE(LTEQ);
        BINOP_CASE(GTEQ);

#undef BINOP_CASE

        case SSA_OP_BITCAST: {
            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 source_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            assert(fn->registers.count >= dest_reg);
            Type* target_type = fn->registers[dest_reg].type;
            assert(target_type);

            string_builder_append(sb, "  %%%u = BITCAST %%%u <%s>\n", dest_reg, source_reg,
                                  temp_type_string(inst, target_type).data);
            break;
        }

        case SSA_OP_TRUNC: {
            u8 size = *(u8*)&bytes[ip];
            ip += sizeof(u8);

            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 operand_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = TRUNC %hhu %%%u\n", dest_reg, size, operand_reg);
            break;
        }

        case SSA_OP_SEXT: {
            u8 dest_size = *(u8*)&bytes[ip];
            ip += sizeof(u8);

            u8 source_size = *(u8*)&bytes[ip];
            ip += sizeof(u8);

            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 operand_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = SEXT %hhu %hhu %%%u\n", dest_reg, dest_size, source_size, operand_reg);
            break;
        }

        case SSA_OP_ZEXT: {
            u8 dest_size = *(u8*)&bytes[ip];
            ip += sizeof(u8);

            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 operand_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = ZEXT %hhu %%%u\n", dest_reg, dest_size, operand_reg);
            break;
        }

        case SSA_OP_ALLOC: {
            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            s64 size = *(s64*)&bytes[ip];
            ip += sizeof(s64);

            assert(dest_reg < fn->allocs.count);
            Type* alloc_type = ast_node_type(fn->allocs[dest_reg].ast_node);

            string_builder_append(sb, "  %%%u = ALLOC %lld <%s>\n", dest_reg, size,
                                  temp_type_string(inst, alloc_type).data);
            break;
        }

        case SSA_OP_GLOB_PTR: {
            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 glob_idx = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            assert(glob_idx < program->globals.count);

            string_builder_append(sb, "  %%%u = GLOB_PTR @%u\n", dest_reg, glob_idx);

            break;
        }

        case SSA_OP_MEMCPY: {
            u32 dest_ptr_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 source_ptr_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            s64 size = *(s64*)&bytes[ip];
            ip += sizeof(s64);

            string_builder_append(sb, "  MEMCPY %%%u %%%u %lld\n", dest_ptr_reg, source_ptr_reg, size);
            break;
        }

        case SSA_OP_STORE_PTR: {
            u8 size_reg = *(u8*)&bytes[ip];
            ip += sizeof(u8);

            u32 ptr_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 value_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  STORE_PTR %hhu %%%u %%%u\n", size_reg, ptr_reg, value_reg);
            break;
        }

        case SSA_OP_LOAD_IM: {
            u32 size = *(u8*)&bytes[ip];
            ip += sizeof(u8);

            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = LOAD_IM %hhu ", dest_reg, size);

            switch (size) {
                default: assert(false); break;
                case 1: {
                    u8 value = *(u8*)&bytes[ip];
                    ip += sizeof(u8);
                    string_builder_append(sb, "%hhu\n", value);
                    break;
                }
                case 2: {
                    u16 value = *(u16*)&bytes[ip];
                    ip += sizeof(u16);
                    string_builder_append(sb, "%hu\n", value);
                    break;
                }
                case 4: {
                    u32 value = *(u32*)&bytes[ip];
                    ip += sizeof(u32);
                    string_builder_append(sb, "%lu\n", value);
                    break;
                }
                case 8: {
                    u64 value = *(u64*)&bytes[ip];
                    ip += sizeof(u64);
                    string_builder_append(sb, "%llu\n", value);
                    break;
                }
            }

            break;
        }

        case SSA_OP_LOAD_PARAM: {
            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 index = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = LOAD_PARAM %u\n", dest_reg, index);
            break;
        }

        case SSA_OP_LOAD_PTR: {
            u8 size_reg = *(u8*)&bytes[ip];
            ip += sizeof(u8);

            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 ptr_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = LOAD_PTR %hhu %%%u\n", dest_reg, size_reg, ptr_reg);
            break;
        }

        case SSA_OP_LOAD_CONST: {
            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 offset_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = LOAD_CONST %u\n", dest_reg, offset_reg);
            break;
        }

        case SSA_OP_LOAD_ENUM: {
            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u64 index = *(u64*)&bytes[ip];
            ip += sizeof(u64);

            Type* enum_type = fn->registers[dest_reg].type;
            assert(enum_type->kind == Type_Kind::ENUM);

            u64 value;
            switch (enum_type->bit_size) {
                default: assert(false && "Invalid size in SSA_OP_LOAD_ENUM");

                case 8: value = *(u8*)&bytes[ip]; ip += sizeof(u8); break;
                case 16: value = *(u16*)&bytes[ip]; ip += sizeof(u16); break;
                case 32: value = *(u32*)&bytes[ip]; ip += sizeof(u32); break;
                case 64: value = *(u64*)&bytes[ip]; ip += sizeof(u64); break;
            }

            string_builder_append(sb, "  %%%u = LOAD_ENUM %llu %llu\n", dest_reg, index, value);
            break;
        }

        case SSA_OP_STRUCT_OFFSET: {
            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 ptr_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 offset = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u16 index = *(u16*)&bytes[ip];
            ip += sizeof(u16);

            string_builder_append(sb, "  %%%u = STRUCT_OFFSET %%%u %u %hhu\n", dest_reg, ptr_reg, offset, index);
            break;
        }

        case SSA_OP_POINTER_OFFSET: {
            u64 size = *(u64*)&bytes[ip];
            ip += sizeof(u64);

            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 base_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 index_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = POINTER_OFFSET %llu %%%u %%%u\n", dest_reg, size, base_reg, index_reg);
            break;
        }

        case SSA_OP_POINTER_DIFF: {
            u64 size = *(u64*)&bytes[ip];
            ip += sizeof(u64);

            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 left_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 right_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = POINTER_DIFF %llu %%%u %%%u\n", dest_reg, size, left_reg, right_reg);
            break;
        }

        case SSA_OP_PUSH: {
            u32 value_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  PUSH %%%u\n", value_reg);
            break;
        }

        case SSA_OP_POP_N: {
            u32 count = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  POP_N %u\n", count);
            break;
        }

        case SSA_OP_CALL: {
            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 fn_index = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            assert(fn_index >= 0 && fn_index < program->functions.count);
            String name = atom_string(program->functions[fn_index].name);
            string_builder_append(sb, "  %%%u = CALL %%%s\n", dest_reg, name.data);
            break;
        }

        case SSA_OP_CALL_FOREIGN: {
            u32 dest_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 fn_index = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u16 arg_count = *(u16*)&bytes[ip];
            ip += sizeof(u16);

            assert(fn_index >= 0 && fn_index < program->functions.count);
            String name = atom_string(program->functions[fn_index].name);
            string_builder_append(sb, "  %%%u = CALL_FOREIGN %%%s %hu\n", dest_reg, name.data, arg_count);
            break;
        }

        case SSA_OP_RET: {
            u32 value_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  RET %%%u\n", value_reg);
            break;
        }

        case SSA_OP_RET_VOID: {
            string_builder_append(sb, "  RET_VOID\n");
            break;
        }

        case SSA_OP_JMP_IF: {
            u32 cond_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 true_index = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 false_index = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            assert(true_index < fn->blocks.count);
            assert(false_index < fn->blocks.count);

            String true_block_name = atom_string(fn->blocks[true_index].name);
            String false_block_name = atom_string(fn->blocks[false_index].name);

            string_builder_append(sb, "  JMP_IF %%%u [%s] [%s]\n", cond_reg, true_block_name.data, false_block_name.data);
            break;
        }

        case SSA_OP_JMP: {
            u32 block_index = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            String block_name = atom_string(fn->blocks[block_index].name);

            string_builder_append(sb, "  JMP [%s]\n", block_name.data);
            break;
        }

        case SSA_OP_ASSERT: {
            u32 cond_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            u32 string_reg = *(u32*)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  ASSERT %%%u %%%u\n", cond_reg, string_reg);
            break;
        }
    }

    return ip;
}

}
