#include "ssa.h"

#include "ast.h"
#include "type.h"

#include <string_builder.h>

namespace Novo {

void ssa_program_init(SSA_Program *program, Allocator *allocator)
{
    program->allocator = allocator;
    program->entry_fn_index = -1;
    darray_init(allocator, &program->functions);
}

void ssa_function_init(SSA_Program *program, SSA_Function *func, Atom name, u32 param_count)
{
    func->name = name;
    func->register_count = 0;
    func->param_count = param_count;
    darray_init(program->allocator, &func->blocks);

    SSA_Block first_block;
    ssa_block_init(program, func, &first_block, "entry");
    darray_append(&func->blocks, first_block);
}

void ssa_block_init(SSA_Program *program, SSA_Function *func, SSA_Block *block, Atom name)
{
    block->name = name;
    darray_init(program->allocator, &block->bytes);
}

void ssa_block_init(SSA_Program *program, SSA_Function *func, SSA_Block *block, const char *name)
{
    return ssa_block_init(program, func, block, atom_get(name));
}

bool ssa_emit_function(SSA_Program *program, AST_Declaration *decl)
{
    SSA_Function func;
    assert(decl->ident);

    ssa_function_init(program, &func, decl->ident->atom, decl->function.params.count);

    auto scope = decl->function.scope;

    for (s64 i = 0; i < decl->function.body.count; i++) {
        auto stmt = decl->function.body[i];

        ssa_emit_statement(program, &func, 0, stmt, scope);
    }

    if (decl->ident->atom == atom_get("main")) {
        assert(program->entry_fn_index == -1);
        program->entry_fn_index = program->functions.count;
    }
    darray_append(&program->functions, func);

    return true;
}

u32 ssa_register_create(SSA_Function *function)
{
    assert(function->register_count != U32_MAX);
    return function->register_count++;
}

void ssa_emit_statement(SSA_Program *program, SSA_Function *func, s64 block_index, AST_Statement *stmt, Scope *scope)
{
    switch (stmt->kind) {

        case AST_Statement_Kind::INVALID: assert(false); break;
        case AST_Statement_Kind::IMPORT: assert(false); break;
        case AST_Statement_Kind::DECLARATION: assert(false); break;
        case AST_Statement_Kind::ASSIGNMENT: assert(false); break;
        case AST_Statement_Kind::CALL: assert(false); break;

        case AST_Statement_Kind::RETURN: {
            if (stmt->return_expr) {
                u32 value_reg = ssa_emit_expression(program, func, block_index, stmt->return_expr, scope);

                ssa_emit_op(program, func, block_index, SSA_OP_RET);
                ssa_emit_32(program, func, block_index, value_reg);

            } else {
                assert(false);
            }
        }
    }
}

s64 ssa_emit_expression(SSA_Program *program, SSA_Function *func, s64 block_index, AST_Expression *expr, Scope *scope)
{
    s64 result = -1;

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            AST_Declaration *decl = expr->identifier->decl;
            assert(decl);
            assert(decl->kind == AST_Declaration_Kind::VARIABLE);
            assert(decl->flags & AST_DECL_FLAG_PARAM);

            ssa_emit_op(program, func, block_index, SSA_OP_LOAD_PARAM);

            result = ssa_register_create(func);
            ssa_emit_32(program, func, block_index, result);

            assert(decl->variable.index >= 0);
            u32 param_index = decl->variable.index;
            ssa_emit_32(program, func, block_index, param_index);
            break;
        }

        case AST_Expression_Kind::BINARY: {
            u32 left = ssa_emit_expression(program, func, block_index, expr->binary.lhs, scope);
            u32 right = ssa_emit_expression(program, func, block_index, expr->binary.rhs, scope);

            assert(expr->binary.op == '+');

            ssa_emit_op(program, func, block_index, SSA_OP_ADD);

            result = ssa_register_create(func);
            ssa_emit_32(program, func, block_index, result);

            ssa_emit_32(program, func, block_index, left);
            ssa_emit_32(program, func, block_index, right);

            break;
        }

        case AST_Expression_Kind::CALL: {

            // Only support calling via identifier for now...
            assert(expr->call.base->kind == AST_Expression_Kind::IDENTIFIER);
            auto name = expr->call.base->identifier->atom;

            bool found = false;
            u32 fn_index;
            for (s64 i = 0; i < program->functions.count; i++) {
                if (program->functions[i].name == name) {
                    found = true;
                    fn_index = i;
                    break;
                }
            }
            assert(found);

            for (s64 i = 0; i < expr->call.args.count; i++) {
                u32 arg_reg = ssa_emit_expression(program, func, block_index, expr->call.args[i], scope);

                ssa_emit_op(program, func, block_index, SSA_OP_PUSH);
                ssa_emit_32(program, func, block_index, arg_reg);
            }

            assert(expr->resolved_type->kind != Type_Kind::VOID);

            ssa_emit_op(program, func, block_index, SSA_OP_CALL);

            result = ssa_register_create(func);
            ssa_emit_32(program, func, block_index, result);

            ssa_emit_32(program, func, block_index, fn_index);

            ssa_emit_op(program, func, block_index, SSA_OP_POP_N);
            ssa_emit_32(program, func, block_index, expr->call.args.count);
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
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;
    }

    return result;
}

void ssa_emit_op(SSA_Program *program, SSA_Function *func, s64 block_index, SSA_Op op)
{
    darray_append(&func->blocks[block_index].bytes, (u8)op);
}

void ssa_emit_32(SSA_Program *program, SSA_Function *func, s64 block_index, u32 value)
{
    auto bytes = &func->blocks[block_index].bytes;

    // Little endian
    darray_append(bytes, (u8)(value & 0xFF));
    darray_append(bytes, (u8)((value >> 8) & 0xFF));
    darray_append(bytes, (u8)((value >> 16) & 0xFF));
    darray_append(bytes, (u8)((value >> 24) & 0xFF));
}

void ssa_emit_64(SSA_Program *program, SSA_Function *func, s64 block_index, u64 value)
{
    auto bytes = &func->blocks[block_index].bytes;

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

        for (s64 bi = 0; bi < fn->blocks.count; bi++) {
            SSA_Block *block = &fn->blocks[bi];
            string_builder_append(sb, " %s:\n", atom_string(block->name).data);

            s64 ip = 0;

            while (ip < block->bytes.count) {
                ip = ssa_print_instruction(sb, program, ip, block->bytes);
            }
        }
    }
}

s64 ssa_print_instruction(String_Builder *sb, SSA_Program *program, s64 ip, Array_Ref<u8> bytes)
{
    assert(bytes.count);

    SSA_Op op = (SSA_Op)bytes[ip++];

    switch (op) {

        default: assert(false && "Invalid or unhandled instruction in ssa_print_instruction");

        case SSA_OP_NOP: assert(false); break;

        case SSA_OP_LOAD_IM: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u64 value = *(u64 *)&bytes[ip];
            ip += sizeof(u64);

            string_builder_append(sb, "  %%%u = LOAD_IM %llu\n", dest_reg, value);

            break;
        }

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

        case SSA_OP_LOAD_PARAM: {
            u32 dest_reg = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            u32 index = *(u32 *)&bytes[ip];
            ip += sizeof(u32);

            string_builder_append(sb, "  %%%u = LOAD_PARAM %u\n", dest_reg, index);
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
    }

    return ip;
}

}
