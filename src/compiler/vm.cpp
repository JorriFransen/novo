#include "vm.h"

#include <dyncall.h>

#include <containers/darray.h>
#include <cstdio>
#include <memory/allocator.h>

#include "ssa.h"

#include <assert.h>
#include <string.h>

namespace Novo {

void vm_init(VM* vm, Allocator* allocator)
{
    vm->allocator = allocator;
    vm->current_program = nullptr;
    vm->fn_index = 0;
    vm->block_index = 0;

    vm->register_offset = 0;
    vm->sp = 0;
    vm->bp = 0;


    vm->register_count = 64;
    vm->stack_size = 32;

    vm->registers = allocate_array<u64>(allocator, vm->register_count);
    vm->stack = allocate_array<u64>(allocator, vm->stack_size);

    vm->dyncall_vm = dcNewCallVM(4096);
    dcMode(vm->dyncall_vm, DC_CALL_C_DEFAULT);
    dcReset(vm->dyncall_vm);
}

template <typename T>
NINLINE T vm_fetch(SSA_Block* block, s64* ip)
{
    assert(*ip + (s64)sizeof(T) - 1 < block->bytes.count);
    T result = *(T*)&block->bytes[*ip];
    *ip += sizeof(T);
    return result;
}

NINLINE void vm_stack_push(VM* vm, u64 value)
{
    assert(vm->sp < vm->stack_size);
    vm->stack[vm->sp] = value;
    vm->sp += 1;
}

NINLINE u64 vm_stack_pop(VM* vm)
{
    assert(vm->sp >= 1);
    u64 result = vm->stack[vm->sp - 1];
    vm->sp -= 1;
    return result;
}

NINLINE u64 vm_stack_top(VM *vm)
{
    assert(vm->sp >= 1);
    return vm->stack[vm->sp - 1];
}

NINLINE void vm_set_register(VM* vm, u32 reg, u64 value)
{
    auto index = vm->register_offset + reg;
    assert(index < vm->register_count);
    vm->registers[index] = value;
}

NINLINE u64 vm_get_register(VM* vm, u32 reg)
{
    auto index = vm->register_offset + reg;
    assert(index < vm->register_count);
    return vm->registers[vm->register_offset + reg];
}

u64 vm_run(VM* vm, SSA_Program* program)
{
    assert(program->entry_fn_index >= 0 && program->entry_fn_index < program->functions.count);

    vm->current_program = program;
    vm->fn_index = program->entry_fn_index;
    vm->block_index = 0;

    vm_stack_push(vm, 0); // dummy bp
    vm_stack_push(vm, 0); // dummy dest reg
    vm_stack_push(vm, 0); // dummy ip
    vm_stack_push(vm, 0); // dummy fn_index
    vm_stack_push(vm, 0); // dummy block_index
    vm_stack_push(vm, 0); // dummy register_offset

    return vm_run(vm);
}

u64 vm_run(VM* vm)
{
    SSA_Function* fn = &vm->current_program->functions[vm->fn_index];
    SSA_Block* block = &fn->blocks[vm->block_index];

    s64 ip = 0;

    while (ip < block->bytes.count) {

        SSA_Op op = vm_fetch<SSA_Op>(block, &ip);

        switch (op) {

            case SSA_OP_NOP: assert(false); break;

#define BINOP_CASE(op_name, op) case SSA_OP_##op_name: { \
    u8 size = vm_fetch<u8>(block, &ip); \
    assert(size); \
    u32 dest_reg = vm_fetch<u32>(block, &ip); \
    u32 left_reg = vm_fetch<u32>(block, &ip); \
    u32 right_reg = vm_fetch<u32>(block, &ip); \
    u64 left_value = vm_get_register(vm, left_reg); \
    u64 right_value = vm_get_register(vm, right_reg); \
    u64 result; \
    switch (size) { \
        default: assert(false); break; \
        case 1: result = (u8)left_value op (u8)right_value; break; \
        case 2: result = (u16)left_value op (u16)right_value; break; \
        case 4: result = (u32)left_value op (u32)right_value; break; \
        case 8: result = (u64)left_value op (u64)right_value; break; \
    } \
    vm_set_register(vm, dest_reg, result); \
    break; \
}

            BINOP_CASE(ADD, +);
            BINOP_CASE(SUB, -);
            BINOP_CASE(MUL, *);
            BINOP_CASE(DIV, /);
            BINOP_CASE(LT, <);
            BINOP_CASE(GT, >);
            BINOP_CASE(EQ, ==);
            BINOP_CASE(NEQ, !=);
            BINOP_CASE(GTEQ, >=);
            BINOP_CASE(LTEQ, <=);

#undef BINOP_CASE

            case SSA_OP_MEMCPY: {
                u32 dest_ptr_reg = vm_fetch<u32>(block, &ip);
                u32 src_ptr_reg = vm_fetch<u32>(block, &ip);
                u32 size = vm_fetch<u32>(block, &ip);

                auto dest = (void*)vm_get_register(vm, dest_ptr_reg);
                auto src = (void*)vm_get_register(vm, src_ptr_reg);

                memcpy(dest, src, size);
                break;
            }

            case SSA_OP_ALLOC: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 size = vm_fetch<u32>(block, &ip);

                auto ptr = allocate(vm->allocator, size);
                vm_set_register(vm, dest_reg, (u64)ptr);
                break;
            }

            case SSA_OP_STORE_PTR: {
                u8 size = vm_fetch<u8>(block, &ip);
                u32 ptr_reg = vm_fetch<u32>(block, &ip);
                u32 value_reg = vm_fetch<u32>(block, &ip);

                u64 ptr_value = vm_get_register(vm, ptr_reg);
                u64 value = vm_get_register(vm, value_reg);

                switch (size) {
                    default: assert(false); break;
                    case 1: *(u8*)ptr_value = (u8)value;
                    case 2: *(u16*)ptr_value = (u16)value;
                    case 4: *(u32*)ptr_value = (u32)value;
                    case 8: *(u64*)ptr_value = (u64)value;
                }
                break;
            }

            case SSA_OP_LOAD_IM: {
                u8 size = vm_fetch<u8>(block, &ip);
                u32 dest_reg = vm_fetch<u32>(block, &ip);

                u64 value;
                switch (size) {
                    default: assert(false); break;
                    case 1: value = vm_fetch<u8>(block, &ip); break;
                    case 2: value = vm_fetch<u16>(block, &ip); break;
                    case 4: value = vm_fetch<u32>(block, &ip); break;
                    case 8: value = vm_fetch<u64>(block, &ip); break;
                }

                vm_set_register(vm, dest_reg, value);
                break;
            }

            case SSA_OP_LOAD_PARAM: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 param_index = vm_fetch<u32>(block, &ip);

                u64 value = vm->stack[vm->bp - fn->param_count + param_index];

                vm_set_register(vm, dest_reg, value);
                break;
            }

            case SSA_OP_LOAD_PTR: {
                u8 size = vm_fetch<u8>(block, &ip);
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 ptr_reg = vm_fetch<u32>(block, &ip);

                u64 value;
                u64 ptr_value = vm_get_register(vm, ptr_reg);

                switch (size) {
                    default: assert(false); break;
                    case 1: value = *(u8*)ptr_value; break;
                    case 2: value = *(u16*)ptr_value; break;
                    case 4: value = *(u32*)ptr_value; break;
                    case 8: value = *(u64*)ptr_value; break;
                }

                vm_set_register(vm, dest_reg, value);
                break;
            }

            case SSA_OP_LOAD_CONST: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 offset = vm_fetch<u32>(block, &ip);

                vm_set_register(vm, dest_reg, (u64)&vm->current_program->constant_memory[offset]);
                break;
            }

            case SSA_OP_STRUCT_OFFSET: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 ptr_reg = vm_fetch<u32>(block, &ip);
                u32 offset = vm_fetch<u32>(block, &ip);
                /*u16 index =*/ vm_fetch<u16>(block, &ip);

                u8* ptr = (u8*)vm_get_register(vm, ptr_reg);

                u8* result = ptr + offset;

                vm_set_register(vm, dest_reg, (u64)result);
                break;
            }

            case SSA_OP_PUSH: {
                u32 value_reg = vm_fetch<u32>(block, &ip);

                u64 value = vm_get_register(vm, value_reg);

                vm_stack_push(vm, value);
                break;
            }

            case SSA_OP_POP_N: {
                u32 n = vm_fetch<u32>(block, &ip);
                vm->sp -= n;
                break;
            }

            case SSA_OP_CALL: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 fn_index = vm_fetch<u32>(block, &ip);

                auto old_fn = fn;

                assert(fn_index >= 0 && fn_index < vm->current_program->functions.count);

                auto new_bp = vm->sp;
                vm_stack_push(vm, vm->bp);
                vm_stack_push(vm, dest_reg);
                vm_stack_push(vm, ip);
                vm_stack_push(vm, vm->fn_index);
                vm_stack_push(vm, vm->block_index);
                vm_stack_push(vm, vm->register_offset);

                vm->bp = new_bp;
                ip = 0;

                vm->fn_index = fn_index;
                fn = &vm->current_program->functions[fn_index];
                vm->block_index = 0;
                block = &fn->blocks[0];
                vm->register_offset += old_fn->register_count;

                break;
            }

            case SSA_OP_CALL_FOREIGN: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 fn_index = vm_fetch<u32>(block, &ip);
                assert(fn_index >= 0 && fn_index < vm->current_program->functions.count);
                // auto old_fn = fn;
                fn = &vm->current_program->functions[fn_index];
                assert(fn->foreign);
                assert(dest_reg);

                u64 arg = vm_stack_top(vm);
                assert(fn->name == atom_get("putchar"));
                u64 result = putchar(arg);

                vm_set_register(vm, dest_reg, result);

                //
                //
                // auto new_bp = vm->sp;
                // vm_stack_push(vm, vm->bp);
                // vm_stack_push(vm, dest_reg);
                // vm_stack_push(vm, ip);
                // vm_stack_push(vm, vm->fn_index);
                // vm_stack_push(vm, vm->block_index);
                // vm_stack_push(vm, vm->register_offset);
                //
                // vm->bp = new_bp;
                // ip = 0;
                //
                // vm->fn_index = fn_index;
                // fn = &vm->current_program->functions[fn_index];
                // vm->block_index = 0;
                // block = &fn->blocks[0];
                // vm->register_offset += old_fn->register_count;

                break;
            }

            case SSA_OP_RET: {
                u32 value_reg = vm_fetch<u32>(block, &ip);

                u64 value = vm_get_register(vm, value_reg);

                vm->register_offset = vm_stack_pop(vm);
                vm->block_index = vm_stack_pop(vm);
                vm->fn_index = vm_stack_pop(vm);
                ip = vm_stack_pop(vm);

                // This register is the callers frame
                u32 dest_reg = vm_stack_pop(vm);

                auto old_bp = vm->bp;
                vm->bp = vm_stack_pop(vm);

                if (old_bp == vm->bp) {
                    assert(vm->sp == 0);
                    return value;

                } else {

                    fn = &vm->current_program->functions[vm->fn_index];
                    block = &fn->blocks[vm->block_index];

                    vm->registers[vm->register_offset + dest_reg] = value;
                }

                break;
            }

            case SSA_OP_RET_VOID: {


                vm->register_offset = vm_stack_pop(vm);
                vm->block_index = vm_stack_pop(vm);
                vm->fn_index = vm_stack_pop(vm);
                ip = vm_stack_pop(vm);

                // This register is the callers frame
                /*u32 dest_reg =*/ vm_stack_pop(vm);

                auto old_bp = vm->bp;
                vm->bp = vm_stack_pop(vm);

                if (old_bp == vm->bp) {
                    assert(vm->sp == 0);
                    assert(false); // Exit vm

                } else {

                    fn = &vm->current_program->functions[vm->fn_index];
                    block = &fn->blocks[vm->block_index];

                }

                break;
            }


            case SSA_OP_JMP_IF: {
                u32 cond_reg = vm_fetch<u32>(block, &ip);
                u32 true_block_index = vm_fetch<u32>(block, &ip);
                u32 false_block_index = vm_fetch<u32>(block, &ip);

                fn = &vm->current_program->functions[vm->fn_index];
                assert(true_block_index < fn->blocks.count);
                assert(false_block_index < fn->blocks.count);

                u64 cond = vm_get_register(vm, cond_reg);
                if (cond) {
                    vm->block_index = true_block_index;
                } else {
                    vm->block_index = false_block_index;
                }

                block = &fn->blocks[vm->block_index];
                ip = 0;
                break;
            }

            case SSA_OP_JMP: {
                u32 block_index = vm_fetch<u32>(block, &ip);

                fn = &vm->current_program->functions[vm->fn_index];
                assert(block_index < fn->blocks.count);

                vm->block_index = block_index;
                block = &fn->blocks[vm->block_index];
                ip = 0;
                break;
            }
        }
    }

    assert(false);
    return 0;
}

}
