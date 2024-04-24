#pragma once

#include "ast.h"
#include "atom.h"
#include "source_pos.h"

#include <containers/darray.h>
#include <containers/hash_table.h>
#include <defines.h>
#include <nstring.h>

namespace Novo {

struct Allocator;
struct AST_Declaration;
struct AST_Expression;
struct AST_Statement;
struct Instance;
struct Scope;
struct String_Builder;
struct Type;

enum SSA_Op : u8
{
    SSA_OP_NOP,

    SSA_OP_ADD,             // ADD [8-bit size reg] [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]
    SSA_OP_SUB,             // SUB [8-bit size reg] [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]
    SSA_OP_MUL,             // MUL [8-bit size reg] [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]
    SSA_OP_DIV,             // DIV [8-bit size reg] [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]

    SSA_OP_LT,              // LT [8-bit size reg] [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]
    SSA_OP_GT,              // GT [8-bit size reg] [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]
    SSA_OP_EQ,              // EQ [8-bit size reg] [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]
    SSA_OP_NEQ,             // NEQ [8-bit size reg] [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]
    SSA_OP_LTEQ,            // LTEQ [8-bit size reg] [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]
    SSA_OP_GTEQ,            // GTEQ [8-bit size reg] [32-bit dest reg] [32-bit left operand reg] [32-bit right operand reg]

    SSA_OP_BITCAST,         // BITCAST [32-bit dest reg] [32-bit operand reg]
    SSA_OP_TRUNC,           // TRUNC [8-bit size reg] [32-bit dest reg] [32-bit operand reg]
    SSA_OP_SEXT,            // SEXT [8-bit dest size reg] [8-bit src size reg] [32-bit dest reg] [32-bit operand reg]
    SSA_OP_ZEXT,            // ZEXT [8-bit dest size reg] [32-bit dest reg] [32-bit operand reg]

    SSA_OP_ALLOC,           // ALLOC [32-bit dest reg] [64-bit size in bytes]
    SSA_OP_GLOB_PTR,        // GLOB_PTR [32-bit dest reg] [32-bit global index]

    SSA_OP_MEMCPY,          // MEMCPY [32-bit dest ptr reg] [32-bit source ptr reg] [64-bit size in bytes]

    SSA_OP_STORE_PTR,       // STORE_PTR [8-bit size reg] [32-bit ptr reg] [32-bit value reg]

    SSA_OP_LOAD_IM,         // LOAD_IM [8-bit size] [32-bit dest reg] [immediate]
    SSA_OP_LOAD_PARAM,      // LOAD_PARAM [32-bit dest reg] [32-bit param index]
    SSA_OP_LOAD_PTR,        // LOAD_PTR [8-bit size reg] [32-bit dest reg] [32-bit ptr_reg]
    SSA_OP_LOAD_CONST,      // LOAD_CONST [32-bit dest reg] [32-bit offset]

    SSA_OP_STRUCT_OFFSET,   // STRUCT_OFFSET [32-bit dest reg] [32-bit base ptr reg] [32-bit offset] [16-bit index]
    SSA_OP_POINTER_OFFSET,  // POINTER_OFFSET [64-bit size in bytes] [32-bit dest reg] [32-bit base ptr reg] [32-bit index reg]
    SSA_OP_POINTER_DIFF,    // POINTER_DIFF [64-bit size in bytes] [32-bit dest reg] [32-bit left reg] [32-bit right reg]

    SSA_OP_PUSH,            // PUSH [32-bit value reg]
    SSA_OP_POP_N,           // POP_N [32-bit count]

    SSA_OP_CALL,            // CALL [32-bit dest reg] [32-bit function index]
    SSA_OP_CALL_FOREIGN,    // CALL_FOREIGN [32-bit dest reg] [32-bit function index] [16-bit arg count]
    SSA_OP_RET,             // RET [32-bit value reg]
    SSA_OP_RET_VOID,        // RET_VOID

    SSA_OP_JMP_IF,          // JMP_IF [32-bit cond reg] [32-bit true block] [32-bit false block]
    SSA_OP_JMP,             // JMP [32-bit block]

    SSA_OP_ASSERT,          // ASSERT [32-bit cond reg] [32-bit string register]
};

struct SSA_Block
{
    Atom name;
    Atom base_name;
    DArray<u8> bytes;

    bool exits;

    DArray<u32> incoming;
    s64 next_index; // This is used to reorder blocks for printing
};

struct AST_Node;

struct SSA_Alloc
{
    AST_Node ast_node;
    u32 alloc_reg;
};


struct SSA_Function
{
    Atom name;
    u32 register_count;
    u32 param_count;

    Type *type;

    DArray<SSA_Block> blocks;

    // TODO: Use hash table if this gets too big?
    DArray<SSA_Alloc> allocs;
    u32 total_alloc_size;

    DArray<Type*> register_types;

    bool sret;
    bool foreign;

    u32 ffi_index;
    Source_Pos source_pos;
};

struct SSA_Constant
{
    Type *type;
    u32 offset;

    AST_Expression* from_expression;
};

struct SSA_Global
{
    Atom name;
    Type* type;
    u32 initializer_constant_index;
    u64 offset;
    Source_Pos source_pos;
};

struct SSA_Assert_Pos
{
    u32 offset;
    u32 fn_index;
    u32 block_index;
};

struct SSA_Constant_Reference
{
    AST_Node ast_node;
    u32 const_index;
};

struct SSA_Program
{
    Allocator* allocator;

    s64 entry_fn_index;

    DArray<u8> constant_memory;
    DArray<SSA_Constant> constants;
    DArray<s64> constant_patch_offsets;
    DArray<SSA_Function> functions;

    DArray<SSA_Constant_Reference> constant_references;

    DArray<SSA_Global> globals;
    s64 globals_size;

    Hash_Table<SSA_Assert_Pos, Source_Pos> instruction_origin_positions;
};

struct SSA_Builder;

NAPI u64 hash_key(SSA_Assert_Pos key);
NAPI bool operator==(const SSA_Assert_Pos& l, const SSA_Assert_Pos& r);

NAPI void ssa_program_init(SSA_Program* program, Allocator* allocator);
NAPI void ssa_program_free(SSA_Program* program);

NAPI void ssa_function_init(Instance* inst, SSA_Program* program, SSA_Function* func, AST_Declaration *decl);
NAPI void ssa_function_init(Instance* inst, SSA_Program* program, SSA_Function* func, Type* type, Atom name, bool foreign, Source_Pos source_pos);
NAPI void ssa_block_init(SSA_Program* program, SSA_Function* func, SSA_Block* block, Atom name);
NAPI void ssa_block_init(SSA_Program* program, SSA_Function* func, SSA_Block* block, const char* name);

NAPI void ssa_global_variable_init(Instance* inst, SSA_Program* program, SSA_Global* glob, AST_Declaration* decl, u64 offset);
NAPI void ssa_global_variable_init(SSA_Global* glob, Type* type, Atom name, u32 init_const_index, Source_Pos source_pos, u64 offset);

NAPI u32 ssa_block_create(SSA_Program* program, SSA_Function* function, const char* name);
NAPI u32 ssa_block_create(SSA_Builder* builder, const char* name);
NAPI u32 ssa_register_create(SSA_Builder* builder, Type* type);

NAPI bool ssa_emit_function(Instance* inst, SSA_Program* program, AST_Declaration* decl);
NAPI bool ssa_emit_global_variable(Instance* inst, SSA_Program* program, AST_Declaration* decl);

NAPI s64 ssa_emit_run_wrapper(Instance* inst, SSA_Program* program, AST_Node node, Scope* scope);

NAPI bool ssa_find_function(SSA_Program* program, Atom atom, u32* index);

NAPI bool ssa_find_global_variable(SSA_Program* program, Atom atom, u32* index);

NAPI bool ssa_find_alloc(SSA_Builder* builder, AST_Node* node, u32* result);
NAPI bool ssa_find_alloc(SSA_Builder* builder, AST_Declaration* decl, u32* result);
NAPI bool ssa_find_alloc(SSA_Builder* builder, AST_Expression* expr, u32* result);

NAPI void ssa_set_insert_point(SSA_Builder* builder, u32 new_block_index);
NAPI bool ssa_block_exits(SSA_Builder* builder, s64 block_index);

NAPI void ssa_emit_statement(SSA_Builder* builder, AST_Statement* stmt, Scope* scope);
NAPI u32 ssa_emit_lvalue(SSA_Builder* builder, AST_Expression* lvalue_expr, Scope* scope);
NAPI s64 ssa_emit_expression(SSA_Builder* builder, AST_Expression* expr, Scope* scope);

NAPI u32 ssa_emit_global_pointer(SSA_Builder *builder, u32 global_index);
NAPI u32 ssa_emit_bitcast(SSA_Builder* builder, Type* from_type, Type* to_type, u32 operand_reg);
NAPI u32 ssa_emit_trunc(SSA_Builder* builder, Type* target_type, u32 operand_reg);
NAPI u32 ssa_emit_sext(SSA_Builder* builder, Type* target_type, s64 source_bit_size, u32 operand_reg);
NAPI u32 ssa_emit_zext(SSA_Builder* builder, Type* target_type, u32 operand_reg);
NAPI u32 ssa_emit_alloc(SSA_Builder* builder, Type* type);
NAPI void ssa_emit_memcpy(SSA_Builder* builder, u32 dest_ptr_reg, u32 src_ptr_reg, s64 bit_size);
NAPI void ssa_emit_store_ptr(SSA_Builder* builder, s64 bit_size, u32 dest_reg, u32 source_reg);
NAPI u32 ssa_emit_load_immediate(SSA_Builder* builder, Type* type, u64 immediate_value);
NAPI u32 ssa_emit_load_param(SSA_Builder* builder, u32 param_index);
NAPI u32 ssa_emit_load_ptr(SSA_Builder* builder, Type* type, u32 ptr_reg);
NAPI u32 ssa_emit_load_constant(SSA_Builder *builder, u32 index);
NAPI u32 ssa_emit_struct_offset(SSA_Builder* builder, u32 struct_ptr_reg, Type* struct_type, s64 index);
NAPI u32 ssa_emit_pointer_offset(SSA_Builder* builder, Type* pointer_type, u32 base_reg, u32 index_reg);
NAPI u32 ssa_emit_pointer_diff(SSA_Builder* builder, s64 pointee_bit_size, u32 left_reg, u32 right_reg);
NAPI void ssa_emit_jmp_if(SSA_Builder* builder, u32 cond_reg, u32 true_block, u32 false_block);
NAPI void ssa_emit_jmp(SSA_Builder* builder, u32 block);

NAPI u32 ssa_emit_cast(SSA_Builder* builder, Type* from_type, Type* to_type, u32 operand_reg);
NAPI u32 ssa_emit_integer_integer_cast(SSA_Builder* builder, Type* from_type, Type* to_type, u32 operand_reg);

NAPI u32 ssa_emit_op(SSA_Builder* builder, SSA_Op op);

NAPI void ssa_emit_8(SSA_Builder* builder, u8 value);
NAPI void ssa_emit_16(SSA_Builder* builder, u16 value);
NAPI void ssa_emit_32(SSA_Builder* builder, u32 value);
NAPI void ssa_emit_64(SSA_Builder* builder, u64 value);

NAPI void ssa_emit_8(DArray<u8> *bytes, u8 value);
NAPI void ssa_emit_16(DArray<u8> *bytes, u16 value);
NAPI void ssa_emit_32(DArray<u8> *bytes, u32 value);
NAPI void ssa_emit_64(DArray<u8> *bytes, u64 value);


NAPI u32 ssa_emit_load_constant_value(SSA_Builder* builder, AST_Expression* expr, Scope* scope);
NAPI u32 ssa_emit_constant(Instance *inst, SSA_Program* program, AST_Expression* const_expr, DArray<u8>* bytes = nullptr);
NAPI u32 ssa_emit_constant(SSA_Program* program, Array_Ref<u8> bytes, AST_Expression* const_expr);

NAPI Atom ssa_unique_function_name(Instance* inst, SSA_Program* program, String_Ref name);

NAPI String ssa_to_string(Instance* inst, Allocator* allocator, SSA_Program* program);
NAPI void ssa_print(Instance* inst, String_Builder* sb, SSA_Program* program);
NAPI void ssa_print_constant(Instance* inst, String_Builder* sb, SSA_Program* program, u32 index);
NAPI void ssa_print_pointer_value(String_Builder* sb, Type* type, u8* ptr);
NAPI s64 ssa_print_instruction(Instance* inst, String_Builder* sb, SSA_Program* program, SSA_Function* fn, s64 ip, Array_Ref<u8> bytes);

}
