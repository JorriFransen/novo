#pragma once

#include <ffi.h>
#include <defines.h>

namespace Novo {

struct Allocator;
struct Instance;
struct SSA_Program;

struct VM
{
    Allocator* allocator;
    Instance *instance;
    SSA_Program* current_program;
    u32 fn_index;
    u32 block_index;

    u32 register_offset;
    u32 bp;
    u32 sp;
    u32 stack_size;
    u32 register_count;
    s64 constant_memory_size;

    u64* registers;
    u64* stack;
    u8* constant_memory;

    FFI ffi;
};

NAPI void vm_init(VM* vm, Allocator* allocator, Instance* inst);
NAPI u64 vm_run(VM* vm, SSA_Program* program);
NAPI u64 vm_run(VM* vm);

}
