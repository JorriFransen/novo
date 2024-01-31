#pragma once

#include "atom.h"

#include <containers/darray.h>
#include <defines.h>
#include <nstring.h>

namespace Novo {

struct Allocator;
struct AST_Declaration;
struct AST_Expression;
struct AST_Statement;
struct Scope;
struct String_Builder;

enum SSA_Op : u8
{
    SSA_OP_NOP,

    SSA_OP_ADD,         // ADD [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]
    SSA_OP_DIV,         // DIV [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]

    SSA_OP_ALLOC,       // ALLOC [32-bit dest reg] [32-bit size]

    SSA_OP_MEMCPY,      // MEMCPY [32-bit dest ptr reg] [32-bit source ptr reg] [32-bit size]

    SSA_OP_STORE_PTR,   // STORE_PTR [32-bit ptr reg] [32-bit value reg]

    SSA_OP_LOAD_IM,     // LOAD_IM [32-bit dest reg] [64-bit immediate]
    SSA_OP_LOAD_PARAM,  // LOAD_PARAM [32-bit dest reg] [32-bit param index]
    SSA_OP_LOAD_PTR,    // LOAD_PTR [32-bit dest reg] [32-bit ptr_reg]

    SSA_OP_STRUCT_OFFSET, // STRUCT_OFFSET [32-bit dest reg] [32-bit base ptr reg] [32-bit offset] [16-bit index]

    SSA_OP_PUSH,        // PUSH [32-bit value reg]
    SSA_OP_POP_N,       // POP_N [32-bit count]

    SSA_OP_CALL,        // CALL [32-bit dest reg] [32-bit function index]
    SSA_OP_RET,         // RET [32-bit value reg]
};

struct SSA_Block
{
    Atom name;
    DArray<u8> bytes;
};

struct AST_Node;
struct SSA_Alloc;

struct SSA_Function
{
    Atom name;
    u32 register_count;
    u32 param_count;
    DArray<SSA_Block> blocks;

    // TODO: Use hash table if this gets too big?
    DArray<SSA_Alloc> allocs;

    bool sret;
};

struct SSA_Program
{
    Allocator *allocator;

    s64 entry_fn_index;
    DArray<SSA_Function> functions;
};

NAPI void ssa_program_init(SSA_Program *program, Allocator *allocator);
NAPI void ssa_function_init(SSA_Program *program, SSA_Function *func, Atom name, u32 param_count, bool sret);
NAPI void ssa_block_init(SSA_Program *program, SSA_Function *func, SSA_Block *block, Atom name);
NAPI void ssa_block_init(SSA_Program *program, SSA_Function *func, SSA_Block *block, const char *name);

NAPI bool ssa_emit_function(SSA_Program *program, AST_Declaration *decl);
NAPI bool ssa_find_function(SSA_Program *program, Atom atom, u32 *index);
NAPI bool ssa_find_alloc(SSA_Function *func, AST_Node *node, u32 *result);
NAPI bool ssa_find_alloc(SSA_Function *func, AST_Declaration *decl, u32 *result);
NAPI bool ssa_find_alloc(SSA_Function *func, AST_Expression *expr, u32 *result);
NAPI u32 ssa_register_create(SSA_Function *function);

NAPI void ssa_emit_statement(SSA_Program *program, SSA_Function *func, s64 block_index, AST_Statement *stmt, Scope *scope);
NAPI u32 ssa_emit_lvalue(SSA_Program *program, SSA_Function *func, s64 block_index, AST_Expression *lvalue_expr, Scope *scope);
NAPI s64 ssa_emit_expression(SSA_Program *program, SSA_Function *func, s64 block_index, AST_Expression *expr, Scope *scope);

NAPI void ssa_emit_op(SSA_Program *program, SSA_Function *func, s64 block_index, SSA_Op op);
NAPI void ssa_emit_8(SSA_Program *program, SSA_Function *func, s64 block_index, u8 value);
NAPI void ssa_emit_16(SSA_Program *program, SSA_Function *func, s64 block_index, u16 value);
NAPI void ssa_emit_32(SSA_Program *program, SSA_Function *func, s64 block_index, u32 value);
NAPI void ssa_emit_64(SSA_Program *program, SSA_Function *func, s64 block_index, u64 value);

NAPI String ssa_to_string(Allocator *allocator, SSA_Program *program);
NAPI void ssa_print(String_Builder *sb, SSA_Program *program);
NAPI s64 ssa_print_instruction(String_Builder *sb, SSA_Program *program, s64 ip, Array_Ref<u8> bytes);

}
