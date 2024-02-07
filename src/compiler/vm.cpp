#include "vm.h"

#include <dyncall.h>

#include <containers/darray.h>
#include <memory/allocator.h>

#include "ffi.h"
#include "instance.h"
#include "ssa.h"
#include "type.h"

#include <assert.h>
#include <string.h>

namespace Novo {

void vm_init(VM* vm, Allocator* allocator, Instance* inst)
{
    vm->allocator = allocator;
    vm->instance = inst;
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

    vm->constant_memory = nullptr;
    vm->constant_memory_size = 0;

    ffi_init(&vm->ffi, allocator);
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

NINLINE u64 vm_stack_peek(VM *vm, s64 offset) {
    assert(offset <= 0);
    assert(vm->sp + offset >= 1);
    return vm->stack[vm->sp - 1 + offset];
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

    for (s64 i = 0; i < program->functions.count; i++) {
        SSA_Function* func = &program->functions[i];
        if (func->foreign) {
            s64 result = ffi_load_function(&vm->ffi, func->name);

            if (result < 0) {

                String name = atom_string(func->name);
                instance_fatal_error(vm->instance, func->source_pos, "FFI unable to load foreign function: '%s'", name.data);
                assert(false);
            }

            func->ffi_index = result;
        }
    }

    vm->current_program = program;
    vm->fn_index = program->entry_fn_index;
    vm->block_index = 0;

    vm_stack_push(vm, 0); // dummy bp
    vm_stack_push(vm, 0); // dummy dest reg
    vm_stack_push(vm, 0); // dummy ip
    vm_stack_push(vm, 0); // dummy fn_index
    vm_stack_push(vm, 0); // dummy block_index
    vm_stack_push(vm, 0); // dummy register_offset

    vm->constant_memory_size = program->constant_memory.count;
    vm->constant_memory = allocate_array<u8>(vm->allocator, vm->constant_memory_size);
    memcpy(vm->constant_memory, program->constant_memory.data, vm->constant_memory_size);

    for (s64 i = 0; i < program->constant_patch_offsets.count; i++) {
        s64 patch_offset = program->constant_patch_offsets[i];
        assert(patch_offset >= 0 && patch_offset < vm->constant_memory_size - sizeof(s64));

        u64* patch_ptr = (u64*)&vm->constant_memory[patch_offset];

        s64 dest_offset = *patch_ptr;
        assert(dest_offset >= 0 && dest_offset < vm->constant_memory_size);
        u8* dest_ptr = &vm->constant_memory[dest_offset];

        *patch_ptr = (u64)dest_ptr;
    }

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
    u64 result = 0; \
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

                u64 value = 0;
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

                u64 value = 0;
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

                vm_set_register(vm, dest_reg, (u64)&vm->constant_memory[offset]);
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
                u16 arg_count = vm_fetch<u16>(block, &ip);

                assert(fn_index >= 0 && fn_index < vm->current_program->functions.count);
                fn = &vm->current_program->functions[fn_index];
                assert(fn->foreign);

                dcReset(vm->ffi.vm);

                s64 arg_offset = -arg_count + 1;
                for (s64 i = 0; i < arg_count; i++, arg_offset++) {

                    Type* arg_type = fn->type->function.param_types[i];
                    u64 arg = vm_stack_peek(vm, arg_offset);

                    switch (arg_type->kind) {
                        case Type_Kind::INVALID: assert(false); break;
                        case Type_Kind::VOID: assert(false); break;

                        case Type_Kind::INTEGER:
                        case Type_Kind::BOOLEAN: {
                            switch (arg_type->bit_size) {
                                default: assert(false); break;
                                case 8: dcArgChar(vm->ffi.vm, arg); break;
                                case 16: dcArgShort(vm->ffi.vm, arg); break;
                                case 32: dcArgInt(vm->ffi.vm, arg); break;
                                case 64: dcArgLongLong(vm->ffi.vm, arg); break;
                            }
                            break;
                        }

                        case Type_Kind::POINTER: dcArgPointer(vm->ffi.vm, (void*)arg); break;

                        case Type_Kind::FUNCTION: assert(false); break;
                        case Type_Kind::STRUCT: assert(false); break;
                    }
                }
                assert(arg_offset == 1);

                void* func_sym = vm->ffi.functions[fn->ffi_index].sym;
                Type *return_type = fn->type->function.return_type;

                u64 result = 0;

                switch (return_type->kind) {
                    default: assert(false); break;
                    case Type_Kind::INVALID: assert(false); break;
                    case Type_Kind::VOID: assert(false); break;

                    case Type_Kind::INTEGER: {
                        switch (return_type->bit_size) {
                            default: assert(false);
                            case 8: result = dcCallChar(vm->ffi.vm, func_sym); break;
                            case 16: result = dcCallShort(vm->ffi.vm, func_sym); break;
                            case 32: result = dcCallInt(vm->ffi.vm, func_sym); break;
                            case 64: result = dcCallLongLong(vm->ffi.vm, func_sym); break;
                        }
                        break;
                    }

                    case Type_Kind::BOOLEAN: assert(false); break;
                    case Type_Kind::POINTER: assert(false); break;
                    case Type_Kind::FUNCTION: assert(false); break;
                    case Type_Kind::STRUCT: assert(false); break;
                }

                vm_set_register(vm, dest_reg, result);

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
