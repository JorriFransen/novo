#pragma once

#include <defines.h>

namespace Novo {

struct Allocator;
struct SSA_Program;

struct VM
{
    Allocator *allocator;
    SSA_Program *current_program;
    u32 fn_index;
    u32 block_index;

    u32 register_offset;
    u32 bp;
    u32 sp;

    u64 *registers;
    u64 *stack;

    u32 register_count;
    u32 stack_size;
};

NAPI void vm_init(VM *vm, Allocator *allocator);
NAPI u64 vm_run(VM *vm, SSA_Program *program);
NAPI u64 vm_run(VM *vm);

}
