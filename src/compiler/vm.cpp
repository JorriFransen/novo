#include "vm.h"

#include <dyncall.h>

#include <containers/darray.h>
#include <containers/hash_table.h>
#include <memory/allocator.h>
#include <memory/temp_allocator.h>
#include <nstring.h>

#include "ast.h"
#include "atom.h"
#include "ffi.h"
#include "instance.h"
#include "source_pos.h"
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
    vm->bp = 0;

    u32 reg_count = NOVO_VM_DEFAULT_REG_COUNT;
    vm->register_count = reg_count;
    vm->registers = allocate_array<u64>(vm->allocator, reg_count);

    stack_init(vm->allocator, &vm->register_stack, NOVO_VM_DEFAULT_REG_STACK_SIZE);

    vm->current_alloc_block = nullptr;
    vm->free_alloc_blocks = nullptr;
    vm->first_alloc_block = {};

    vm->first_alloc_block.mem = allocate_array<u8>(vm->allocator, NOVO_VM_DEFAULT_ALLOC_BLOCK_SIZE);
    vm->first_alloc_block.used = 0;
    vm->first_alloc_block.cap = NOVO_VM_DEFAULT_ALLOC_BLOCK_SIZE;
    vm->first_alloc_block.next = nullptr;
    vm->current_alloc_block = &vm->first_alloc_block;
    vm->free_alloc_blocks = nullptr;

    vm->constant_memory_size = NOVO_VM_DEFAULT_CONST_MEM_SIZE;
    vm->constant_memory = allocate_array<u8>(vm->allocator, vm->constant_memory_size);

    vm->global_memory_size = 0;
    vm->global_memory = nullptr;

    ffi_init(inst, &vm->ffi, allocator);

}

void vm_free(VM* vm)
{
    if (vm->registers) free(vm->allocator, vm->registers);
    stack_free(&vm->register_stack);

    VM_Alloc_Block* block = vm->current_alloc_block;
    while (block && block != &vm->first_alloc_block) {
        VM_Alloc_Block* next = block->next;
        assert(next);

        free(vm->allocator, block);

        block = next;
    }

    assert(block == &vm->first_alloc_block);
    assert(block->next == nullptr);
    free(vm->allocator, vm->first_alloc_block.mem);

    block = vm->free_alloc_blocks;
    while (block) {
        VM_Alloc_Block* next = block->next;

        free(vm->allocator, block);

        block = next;
    }

    if (vm->constant_memory) free(vm->allocator, vm->constant_memory);
    if (vm->global_memory) free(vm->allocator, vm->global_memory);
    ffi_free(&vm->ffi);
}

template <typename T>
NINLINE T vm_fetch(SSA_Block* block, s64* ip)
{
    assert(*ip + (s64)sizeof(T) - 1 < block->bytes.count);
    T result = *(T*)&block->bytes[*ip];
    *ip += sizeof(T);
    return result;
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

VM_Result vm_run(VM* vm, SSA_Program* program)
{
    return vm_run(vm, program, program->entry_fn_index);
}

VM_Result vm_run(VM* vm, SSA_Program* program, s64 fn_index)
{
    assert(fn_index >= 0 && fn_index < program->functions.count);

    vm->current_program = program;
    vm->fn_index = fn_index;
    vm->block_index = 0;

    SSA_Function* fn = &vm->current_program->functions[fn_index];

    if (fn->register_count > vm->register_count) {
        u32 reg_count = vm->register_count;
        while (reg_count < fn->register_count) reg_count *= 2;
        u64 *old_regs = vm->registers;
        vm->registers = allocate_array<u64>(vm->allocator, reg_count);
        memcpy(vm->registers, old_regs, sizeof(vm->registers[0]) * vm->register_count);
        vm->register_count = reg_count;
        free(vm->allocator, old_regs);
    }

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

    stack_push(&vm->register_stack, (u64)0); // dummy bp
    stack_push(&vm->register_stack, (u64)0); // dummy dest reg
    stack_push(&vm->register_stack, (u64)0); // dummy ip
    stack_push(&vm->register_stack, (u64)0); // dummy fn_index
    stack_push(&vm->register_stack, (u64)0); // dummy block_index
    stack_push(&vm->register_stack, (u64)0); // dummy register_offset

    if (program->constant_memory.count > vm->constant_memory_size) {
        s64 new_size = vm->constant_memory_size * 2;
        while (new_size < program->constant_memory.count) new_size *= 2;
        u8* old_consts = vm->constant_memory;
        vm->constant_memory = allocate_array<u8>(vm->allocator, new_size);
        vm->constant_memory_size = new_size;
        free(vm->allocator, old_consts);
    }

    memcpy(vm->constant_memory, program->constant_memory.data, program->constant_memory.count);

    for (s64 i = 0; i < program->constant_patch_offsets.count; i++) {
        s64 patch_offset = program->constant_patch_offsets[i];
        assert(patch_offset >= 0 && patch_offset < vm->constant_memory_size - (s64)sizeof(s64));

        u64* patch_ptr = (u64*)&vm->constant_memory[patch_offset];

        s64 dest_offset = *patch_ptr;
        assert(dest_offset >= 0 && dest_offset < vm->constant_memory_size);
        u8* dest_ptr = &vm->constant_memory[dest_offset];

        *patch_ptr = (u64)dest_ptr;
    }

    if (program->globals_size > vm->global_memory_size) {
        s64 new_size = max((s64)2, vm->global_memory_size);
        while (new_size < program->globals_size) new_size *= 2;

        if (vm->global_memory) free(vm->allocator, vm->global_memory);
        vm->global_memory = allocate_array<u8>(vm->allocator, new_size);
        vm->global_memory_size = new_size;
    }

    for (s64 i = 0; i < program->globals.count; i++) {
        SSA_Global global = program->globals[i];
        SSA_Constant initializer = program->constants[global.initializer_constant_index];
        assert(global.type == initializer.type);

        memcpy(vm->global_memory + global.offset, vm->constant_memory + initializer.offset, global.type->bit_size / 8);
    }

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
                s64 size = vm_fetch<s64>(block, &ip);

                auto dest = (void*)vm_get_register(vm, dest_ptr_reg);
                auto src = (void*)vm_get_register(vm, src_ptr_reg);

                memcpy(dest, src, size);
                break;
            }

            case SSA_OP_BITCAST: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 source_reg = vm_fetch<u32>(block, &ip);

                vm_set_register(vm, dest_reg, vm_get_register(vm, source_reg));

                break;
            }

            case SSA_OP_TRUNC: {
                u8 target_size = vm_fetch<u8>(block, &ip);
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 operand_reg = vm_fetch<u32>(block, &ip);

                u64 value = vm_get_register(vm, operand_reg);

                switch (target_size) {
                    default: assert(false); break;
                    case 1: value = (u8)value; break;
                    case 2: value = (u16)value; break;
                    case 4: value = (u32)value; break;
                    case 8: value = (u64)value; break;
                }

                vm_set_register(vm, dest_reg, value);
                break;
            }

            case SSA_OP_SEXT: {
                u8 target_size = vm_fetch<u8>(block, &ip);
                u8 source_size = vm_fetch<u8>(block, &ip);
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 operand_reg = vm_fetch<u32>(block, &ip);

                u64 value = vm_get_register(vm, operand_reg);

                switch (source_size) {
                    default: assert(false); break;
                    case 1: value = (s64)((s8)value); break;
                    case 2: value = (s64)((s16)value); break;
                    case 4: value = (s64)((s32)value); break;
                    case 8: value = (s64)((s64)value); break;
                }

                switch (target_size) {
                    default: assert(false); break;
                    case 1: value = (u8)value; break;
                    case 2: value = (u16)value; break;
                    case 4: value = (u32)value; break;
                    case 8: value = (u64)value; break;
                }

                vm_set_register(vm, dest_reg, value);
                break;
            }

            case SSA_OP_ZEXT: {
                u8 target_size = vm_fetch<u8>(block, &ip);
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 operand_reg = vm_fetch<u32>(block, &ip);

                u64 value = vm_get_register(vm, operand_reg);

                switch (target_size) {
                    default: assert(false); break;
                    case 1: value = (u8)value; break;
                    case 2: value = (u16)value; break;
                    case 4: value = (u32)value; break;
                    case 8: value = (u64)value; break;
                }

                vm_set_register(vm, dest_reg, value);
                break;
            }

            case SSA_OP_ALLOC: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                s64 size = vm_fetch<s64>(block, &ip);

                if (size > vm->current_alloc_block->cap - vm->current_alloc_block->used) {

                    bool found_free_block = false;
                    if (vm->free_alloc_blocks) {

                        VM_Alloc_Block* last_free_block = nullptr;
                        VM_Alloc_Block* free_block = vm->free_alloc_blocks;
                        while (free_block) {
                            if (free_block->cap >= size) {

                                if (last_free_block) {
                                    last_free_block->next = free_block->next;
                                } else {
                                    vm->free_alloc_blocks = free_block->next;
                                }

                                free_block->next = vm->current_alloc_block;
                                vm->current_alloc_block = free_block;

                                found_free_block = true;
                                break;

                            } else {
                                last_free_block = free_block;
                                free_block = free_block->next;
                            }
                        }

                    }

                    if (!found_free_block) {

                        u32 new_block_size = vm->current_alloc_block->cap;
                        while (new_block_size < size) new_block_size *= 2;

                        u64 alloc_size = new_block_size + sizeof(VM_Alloc_Block);
                        u8* mem = (u8*)allocate(vm->allocator, alloc_size);

                        VM_Alloc_Block* new_block = (VM_Alloc_Block*)mem;
                        new_block->mem = mem + sizeof(VM_Alloc_Block);
                        new_block->used = 0;
                        new_block->cap = new_block_size;
                        new_block->next = vm->current_alloc_block;

                        vm->current_alloc_block = new_block;
                    }
                }

                u8* ptr = vm->current_alloc_block->mem + vm->current_alloc_block->used;
                vm->current_alloc_block->used += size;

                vm_set_register(vm, dest_reg, (u64)ptr);
                break;
            }

            case SSA_OP_GLOB_PTR: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 glob_idx = vm_fetch<u32>(block, &ip);

                assert(glob_idx < program->globals.count);

                u8* ptr = vm->global_memory + program->globals[glob_idx].offset;
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
                    case 1: *(u8*)ptr_value = (u8)value; break;
                    case 2: *(u16*)ptr_value = (u16)value; break;
                    case 4: *(u32*)ptr_value = (u32)value; break;
                    case 8: *(u64*)ptr_value = (u64)value; break;
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

                u64 value = vm->register_stack.buffer[vm->bp - fn->param_count + param_index];

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

            case SSA_OP_POINTER_OFFSET: {
                u64 size = vm_fetch<u64>(block, &ip);
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 base_reg = vm_fetch<u32>(block, &ip);
                u32 index_reg = vm_fetch<u32>(block, &ip);

                u8* base_ptr = (u8*)vm_get_register(vm, base_reg);
                s64 index = vm_get_register(vm, index_reg);

                s64 offset = index * size;
                u8* result_ptr = base_ptr + offset;

                vm_set_register(vm, dest_reg, (u64)result_ptr);
                break;
            }

            case SSA_OP_POINTER_DIFF: {
                u64 size = vm_fetch<u64>(block, &ip);
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 left_reg = vm_fetch<u32>(block, &ip);
                u32 right_reg = vm_fetch<u32>(block, &ip);

                u8* left_ptr = (u8*)vm_get_register(vm, left_reg);
                u8* right_ptr = (u8*)vm_get_register(vm, right_reg);

                s64 byte_diff = left_ptr - right_ptr;
                s64 diff = byte_diff / size;

                vm_set_register(vm, dest_reg, diff);
                break;
            }

            case SSA_OP_PUSH: {
                u32 value_reg = vm_fetch<u32>(block, &ip);

                u64 value = vm_get_register(vm, value_reg);

                stack_push(&vm->register_stack, value);
                break;
            }

            case SSA_OP_POP_N: {
                u32 n = vm_fetch<u32>(block, &ip);
                stack_pop(&vm->register_stack, n);
                break;
            }

            case SSA_OP_CALL: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 fn_index = vm_fetch<u32>(block, &ip);

                auto old_fn = fn;

                assert(fn_index >= 0 && fn_index < vm->current_program->functions.count);

                auto new_bp = vm->register_stack.sp;
                stack_push(&vm->register_stack, (u64)vm->bp);
                stack_push(&vm->register_stack, (u64)dest_reg);
                stack_push(&vm->register_stack, (u64)ip);
                stack_push(&vm->register_stack, (u64)vm->fn_index);
                stack_push(&vm->register_stack, (u64)vm->block_index);
                stack_push(&vm->register_stack, (u64)vm->register_offset);

                vm->bp = new_bp;
                ip = 0;

                vm->fn_index = fn_index;
                fn = &vm->current_program->functions[fn_index];
                vm->block_index = 0;
                block = &fn->blocks[0];
                vm->register_offset += old_fn->register_count;

                u32 regs_required = vm->register_offset + fn->register_count;
                if (regs_required > vm->register_count) {
                    u32 old_count = vm->register_count;
                    u32 new_count = vm->register_count;
                    while (new_count < regs_required) new_count *= 2;

                    auto old_regs = vm->registers;
                    vm->registers = allocate_array<u64>(vm->allocator, new_count);
                    vm->register_count = new_count;
                    memcpy(vm->registers, old_regs, old_count * sizeof(vm->registers[0]));
                    free(vm->allocator, old_regs);
                }

                break;
            }

            case SSA_OP_CALL_FOREIGN: {
                u32 dest_reg = vm_fetch<u32>(block, &ip);
                u32 fn_index = vm_fetch<u32>(block, &ip);
                u16 arg_count = vm_fetch<u16>(block, &ip);

                assert(fn_index >= 0 && fn_index < vm->current_program->functions.count);
                SSA_Function* foreign_fn = &vm->current_program->functions[fn_index];
                assert(foreign_fn->foreign);

                dcReset(vm->ffi.vm);

                bool foreign_vararg = foreign_fn->type->flags & TYPE_FLAG_FOREIGN_VARARG;

                s64 arg_offset = arg_count - 1;
                for (s64 i = 0; i < arg_count; i++, arg_offset--) {

                    Type* arg_type = nullptr;
                    if (i < foreign_fn->type->function.param_types.count) {
                        arg_type = foreign_fn->type->function.param_types[i];
                    } else {
                        assert(foreign_vararg);
                        arg_type = vm->instance->builtin_type_int;
                    }

                    u64 arg = stack_peek(&vm->register_stack, arg_offset);

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
                        case Type_Kind::ENUM: assert(false); break;
                    }
                }
                assert(arg_offset == -1);

                void* func_sym = vm->ffi.functions[foreign_fn->ffi_index].sym;
                Type *return_type = foreign_fn->type->function.return_type;

                u64 result = 0;

                switch (return_type->kind) {
                    default: assert(false); break;
                    case Type_Kind::INVALID: assert(false); break;

                    case Type_Kind::VOID: {
                        dcCallVoid(vm->ffi.vm, func_sym);
                        break;
                    }

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

                    case Type_Kind::POINTER: {
                        result = (u64)dcCallPointer(vm->ffi.vm, func_sym);
                        break;
                    }

                    case Type_Kind::FUNCTION: assert(false); break;
                    case Type_Kind::STRUCT: assert(false); break;
                }

                vm_set_register(vm, dest_reg, result);

                break;
            }

            case SSA_OP_RET: {
                u32 value_reg = vm_fetch<u32>(block, &ip);

                u64 value = vm_get_register(vm, value_reg);

                SSA_Function* old_fn = &vm->current_program->functions[vm->fn_index];

                vm->register_offset = stack_pop(&vm->register_stack);
                vm->block_index = stack_pop(&vm->register_stack);
                vm->fn_index = stack_pop(&vm->register_stack);
                ip = stack_pop(&vm->register_stack);

                // This register is the callers frame
                u32 dest_reg = stack_pop(&vm->register_stack);

                auto old_bp = vm->bp;
                vm->bp = stack_pop(&vm->register_stack);

                u32 remaining_alloc_size = fn->total_alloc_size;
                while (remaining_alloc_size) {

                    u32 chunk_size = min(remaining_alloc_size, vm->current_alloc_block->used);

                    vm->current_alloc_block->used -= chunk_size;
                    remaining_alloc_size -= chunk_size;

                    if (remaining_alloc_size) {
                        VM_Alloc_Block* old_block = vm->current_alloc_block;
                        VM_Alloc_Block* new_block = old_block->next;

                        vm->current_alloc_block = new_block;

                        old_block->next = vm->free_alloc_blocks;
                        vm->free_alloc_blocks = old_block;
                    }
                }

                if (old_bp == vm->bp) {
                    assert(vm->register_stack.sp == 0);
                    return { old_fn->type->function.return_type, value, false };

                } else {

                    fn = &vm->current_program->functions[vm->fn_index];
                    block = &fn->blocks[vm->block_index];

                    vm->registers[vm->register_offset + dest_reg] = value;
                }

                break;
            }

            case SSA_OP_RET_VOID: {


                SSA_Function* old_fn = &vm->current_program->functions[vm->fn_index];

                vm->register_offset = stack_pop(&vm->register_stack);
                vm->block_index = stack_pop(&vm->register_stack);
                vm->fn_index = stack_pop(&vm->register_stack);
                ip = stack_pop(&vm->register_stack);

                // This register is the callers frame
                /*u32 dest_reg =*/ stack_pop(&vm->register_stack);

                auto old_bp = vm->bp;
                vm->bp = stack_pop(&vm->register_stack);

                u32 remaining_alloc_size = fn->total_alloc_size;
                while (remaining_alloc_size) {

                    u32 chunk_size = min(remaining_alloc_size, vm->current_alloc_block->used);

                    vm->current_alloc_block->used -= chunk_size;
                    remaining_alloc_size -= chunk_size;

                    if (remaining_alloc_size) {
                        VM_Alloc_Block* old_block = vm->current_alloc_block;
                        VM_Alloc_Block* new_block = old_block->next;

                        vm->current_alloc_block = new_block;

                        old_block->next = vm->free_alloc_blocks;
                        vm->free_alloc_blocks = old_block;
                    }
                }

                if (old_bp == vm->bp) {
                    assert(vm->register_stack.sp == 0);
                    assert(old_fn->type->function.return_type == vm->instance->builtin_type_void);
                    return { vm->instance->builtin_type_void, 0, false };


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

            case SSA_OP_ASSERT: {
                u32 offset = ip - sizeof(SSA_Op);

                u32 cond_reg = vm_fetch<u32>(block, &ip);
                u32 msg_reg = vm_fetch<u32>(block, &ip);

                u64 cond_value = vm_get_register(vm, cond_reg);

                if (!cond_value) {

                    auto string_ptr = (p_uint_t)vm_get_register(vm, msg_reg);

                    Source_Pos pos;
                    bool found = hash_table_find(&vm->current_program->instruction_origin_positions, { offset, vm->fn_index, vm->block_index }, &pos);
                    assert(found);

                    if (string_ptr != 0) {
                        p_uint_t data_ptr = string_ptr;
                        auto data = *(const char**)data_ptr;

                        // TODO: magic number
                        p_uint_t length_ptr = string_ptr + 8;
                        s64 length = *(s64*)length_ptr;

                        instance_error(vm->instance, pos, "[Bytecode] Assertion failed: \"%.*s\"", (int)length, data);

                    } else {
                        instance_error(vm->instance, pos, "[Bytecode] Assertion failed!");
                    }

                    return { vm->instance->builtin_type_int, 42, true };
                }
                break;
            }
        }
    }

    assert(false);
    return { vm->instance->builtin_type_int, 42, true };
}

AST_Expression* vm_const_expr_from_result(Instance* inst, VM_Result vm_res)
{
    switch (vm_res.type->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;

        case Type_Kind::INTEGER: {
            return ast_integer_literal_expression(inst, vm_res.return_value);
        }

        case Type_Kind::BOOLEAN: assert(false); break;
        case Type_Kind::POINTER: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;

        case Type_Kind::STRUCT: {
            u8* ptr = (u8*)vm_res.return_value;
            return vm_const_expr_from_memory(inst, vm_res.type, ptr);
        }

        case Type_Kind::ENUM: assert(false); break;
    }

    assert(false);
    return nullptr;
}

AST_Expression* vm_const_expr_from_memory(Instance* inst, Type* type, u8* mem)
{
    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;

        case Type_Kind::INTEGER:
        case Type_Kind::BOOLEAN: {
            u64 val = 0;

            switch (type->bit_size) {
                default: assert(false); break;
                case 8: val = *mem; break;
                case 16: val = *(u16*)mem; break;
                case 32: val = *(u32*)mem; break;
                case 64: val = *(u64*)mem; break;
            }

            return ast_integer_literal_expression(inst, val);
        }

        case Type_Kind::POINTER: assert(false); break;
        case Type_Kind::FUNCTION: assert(false); break;

        case Type_Kind::STRUCT: {

            if (type == inst->type_string) {

                auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);

                String temp_str = vm_string_from_memory(inst, &inst->temp_allocator, mem);
                Atom str = atom_get(temp_str);
                temp_allocator_reset(&inst->temp_allocator_data, mark);

                return ast_string_literal_expression(inst, str);

            } else {
                DArray<AST_Expression*> expressions;
                darray_init(&inst->ast_allocator, &expressions, type->structure.members.count);

                for (s64 i = 0; i < type->structure.members.count; i++) {
                    Type_Struct_Member_Resolved member = type->structure.members[i];

                    assert(member.offset % 8 == 0);
                    AST_Expression* mem_expr = vm_const_expr_from_memory(inst, member.type, mem + (member.offset / 8));
                    darray_append(&expressions, mem_expr);
                }

                return ast_compound_expression(inst, expressions);
            }
        }

        case Type_Kind::ENUM: assert(false); break;
    }

    assert(false);
    return nullptr;
}

String vm_string_from_result(Instance* inst, Allocator* allocator, VM_Result vm_res)
{
    u8* ptr = (u8*)vm_res.return_value;
    return vm_string_from_memory(inst, allocator, ptr);
}

String vm_string_from_memory(Instance* inst, Allocator* allocator, u8* mem)
{
    char* data = *(char**)mem;
    s64 length = *(s64*)(mem + inst->pointer_byte_size);

    return string_copy(allocator, data, length);
}

}
