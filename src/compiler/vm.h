#pragma once

#include <containers/stack.h>
#include <defines.h>
#include <ffi.h>
#include <nstring.h>

#define NOVO_VM_DEFAULT_REG_COUNT 128
#define NOVO_VM_DEFAULT_REG_STACK_SIZE 64
#define NOVO_VM_DEFAULT_ALLOC_BLOCK_SIZE 128
#define NOVO_VM_DEFAULT_CONST_MEM_SIZE KIBIBYTE(1)

namespace Novo {

struct Allocator;
struct AST_Expression;
struct Instance;
struct SSA_Program;
struct Type;

struct VM_Alloc_Block
{
    u8* mem;
    u32 used;
    u32 cap;

    VM_Alloc_Block* next;
};

struct VM
{
    Allocator* allocator;
    Instance *instance;
    SSA_Program* current_program;
    u32 fn_index;
    u32 block_index;

    u32 register_offset;
    u32 bp;
    u32 register_count;
    s64 constant_memory_size;
    s64 global_memory_size;

    u64* registers;
    Stack<u64> register_stack;

    VM_Alloc_Block* current_alloc_block;
    VM_Alloc_Block* free_alloc_blocks;
    VM_Alloc_Block first_alloc_block;

    u8* constant_memory;
    u8* global_memory;

    FFI ffi;
};

struct VM_Result
{
    Type* type;
    u64 return_value;
    bool assert_fail;
};

NAPI void vm_init(VM* vm, Allocator* allocator, Instance* inst);
NAPI void vm_free(VM* vm);

NAPI VM_Result vm_run(VM* vm, SSA_Program* program);
NAPI VM_Result vm_run(VM* vm, SSA_Program* program, s64 fn_index);

NAPI AST_Expression* vm_const_expr_from_result(Instance* inst, VM_Result vm_res);
NAPI AST_Expression* vm_const_expr_from_memory(Instance* inst, Type* type, u8* mem);

NAPI String vm_string_from_result(Instance* inst, Allocator* allocator, VM_Result vm_res);
NAPI String vm_string_from_memory(Instance* inst, Allocator* allocator, u8* mem);

}
