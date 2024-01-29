#include "vm.h"

#include <containers/darray.h>
#include <memory/allocator.h>

#include "ssa.h"

#include <assert.h>

namespace Novo {

void vm_init(VM *vm, Allocator *allocator)
{
    vm->allocator = allocator;
    vm->current_program = nullptr;
    vm->fn_index = 0;
    vm->block_index = 0;

    vm->register_offset = 0;
    vm->sp = 0;
    vm->bp = 0;


    vm->register_count = 32;
    vm->stack_size = 32;

    vm->registers = allocate_array<u64>(allocator, vm->register_count);
    vm->stack = allocate_array<u64>(allocator, vm->stack_size);
}

template <typename T>
NINLINE T vm_fetch(SSA_Block *block, s64 *ip)
{
    assert(*ip + (s64)sizeof(T) - 1 < block->bytes.count);
    T result = *(T *)&block->bytes[*ip];
    *ip += sizeof(T);
    return result;
}

NINLINE void vm_stack_push(VM *vm, u64 value)
{
    assert(vm->sp < vm->stack_size);
    vm->stack[vm->sp] = value;
    vm->sp += 1;
}

NINLINE u64 vm_stack_pop(VM *vm)
{
    assert(vm->sp >= 1);
    u64 result = vm->stack[vm->sp - 1];
    vm->sp -= 1;
    return result;
}

NINLINE void vm_set_register(VM *vm, u32 reg, u64 value)
{
    auto index = vm->register_offset + reg;
    assert(index < vm->register_count);
    vm->registers[index] = value;
}

NINLINE u64 vm_get_register(VM *vm, u32 reg)
{
    auto index = vm->register_offset + reg;
    assert(index < vm->register_count);
    return vm->registers[vm->register_offset + reg];
}

u64 vm_run(VM *vm, SSA_Program *program)
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

u64 vm_run(VM *vm)
{
    SSA_Function *fn = &vm->current_program->functions[vm->fn_index];
    SSA_Block *block = &fn->blocks[vm->block_index];

    s64 ip = 0;

    while (ip < block->bytes.count) {

        SSA_Op op = vm_fetch<SSA_Op>(block, &ip);

        switch (op) {

            case SSA_OP_NOP: assert(false); break;

            case SSA_OP_ADD: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 left_reg = vm_fetch<u32>(block, &ip);
                u32 right_reg = vm_fetch<u32>(block, &ip);

                u64 left_value = vm_get_register(vm, left_reg);
                u64 right_value = vm_get_register(vm, right_reg);
                vm_set_register(vm, dest_reg, left_value + right_value);
                break;
            }

            case SSA_OP_ALLOC: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 size = vm_fetch<u32>(block, &ip);

                auto ptr = allocate(vm->allocator, size);
                vm_set_register(vm, dest_reg, (u64)ptr);
                break;
            }

            case SSA_OP_STORE_ALLOC: {
                u32 alloc_reg = vm_fetch<u32>(block, &ip);
                u32 value_reg = vm_fetch<u32>(block, &ip);

                auto ptr = (u64 *)vm_get_register(vm, alloc_reg);
                *ptr = vm_get_register(vm, value_reg);
                break;
            }

            case SSA_OP_STORE_PTR: {
                u32 ptr_reg = vm_fetch<u32>(block, &ip);
                u32 value_reg = vm_fetch<u32>(block, &ip);

                auto ptr = (u64 *)vm_get_register(vm, ptr_reg);
                *ptr = vm_get_register(vm, value_reg);
                break;
            }

            case SSA_OP_LOAD_ALLOC: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 alloc_reg = vm_fetch<u32>(block, &ip);

                auto ptr = (u64 *)vm_get_register(vm, alloc_reg);
                vm_set_register(vm, dest_reg, *ptr);
                break;
            }

            case SSA_OP_LOAD_IM: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u64 value = vm_fetch<u64>(block, &ip);

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
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 ptr_reg = vm_fetch<u32>(block, &ip);

                u64 *ptr = (u64 *)vm_get_register(vm, ptr_reg);
                vm_set_register(vm, dest_reg, *ptr);
                break;
            }

            case SSA_OP_STRUCT_OFFSET: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 ptr_reg = vm_fetch<u32>(block, &ip);
                u32 offset = vm_fetch<u32>(block, &ip);
                /*u16 index =*/ vm_fetch<u16>(block, &ip);

                u8 *ptr = (u8 *)vm_get_register(vm, ptr_reg);

                u8 *result = ptr + offset;

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
        }
    }

    assert(false);
    return 0;
}

}
