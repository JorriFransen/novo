#pragma once

#include <containers/darray.h>
#include <containers/hash_table.h>
#include <defines.h>
#include <memory/allocator.h>
#include <memory/arena.h>
#include <nstring.h>

#include "atom.h"
#include "options.h"
#include "vm.h"

namespace Novo {

struct AST_Declaration;
struct AST_Expression;
struct AST_File;
struct AST_Identifier;
struct AST_Statement;
struct AST_Type_Spec;
struct Parse_Task;
struct Resolve_Task;
struct Run_Task;
struct Scope;
struct Source_Pos;
struct SSA_Program;
struct SSA_Task;
struct Type;
struct Type_Task;

struct Imported_File
{
    Atom name;
    AST_File *ast;
    DArray<u32> newline_offsets;
};

struct Instance
{
    Options options;

    String_Ref cwd = "./";

    Allocator* default_allocator = c_allocator();

    Arena ast_arena;
    Arena temp_arena; // This is reset after each instance cycle, used utility functions like temp_type_string()
    Allocator ast_allocator;
    Allocator temp_allocator;

    DArray<Parse_Task> parse_tasks;
    DArray<Resolve_Task> resolve_tasks;
    DArray<Type_Task> type_tasks;
    DArray<SSA_Task> ssa_tasks;
    DArray<Run_Task> run_tasks;

    Scope* global_scope;

    DArray<Imported_File> imported_files;
    s64 insert_file_index;
    String_Ref inserted_strings_path;

    DArray<Type*> function_types;
    DArray<Type*> struct_types;
    DArray<Type*> enum_types;

    bool fatal_error;

    SSA_Program* ssa_program;
    VM vm;
    VM_Result entry_run_result;

    s64 pointer_byte_size;

    Type* builtin_type_void;
    Type* builtin_type_u8;
    Type* builtin_type_u16;
    Type* builtin_type_u32;
    Type* builtin_type_u64;
    Type* builtin_type_s8;
    Type* builtin_type_s16;
    Type* builtin_type_s32;
    Type* builtin_type_s64;
    Type* builtin_type_int;
    Type* builtin_type_bool;

    Type* builtin_type_cchar;
    Type* builtin_type_cstring;

    String compiler_exe_dir;
    String support_lib_s_path;
    String support_lib_d_path;
    String module_dir;

    String builtin_module_path;
    bool builtin_module_loaded;

    // These types are defined in the builtin module
    Type* type_string;

    Hash_Table<AST_Identifier*, Source_Pos> ident_positions;
    Hash_Table<AST_Declaration*, Source_Pos> decl_positions;
    Hash_Table<AST_Declaration*, Source_Pos> function_body_positions;
    Hash_Table<AST_Statement*, Source_Pos> stmt_positions;
    Hash_Table<AST_Expression*, Source_Pos> expr_positions;
    Hash_Table<AST_Type_Spec*, Source_Pos> ts_positions;
};

NAPI void instance_init(Instance* inst, Options options);
NAPI void instance_free(Instance* inst);

NAPI bool instance_start(Instance* inst);
NAPI bool instance_start(Instance* inst, String_Ref first_file_name, bool builtin_module = false);

NAPI u32 add_insert_string(Instance* inst, Source_Pos insert_pos, String_Ref str);
NAPI String fix_special_characters_in_insert_string(Instance* inst, Allocator* allocator, String_Ref str);

NAPI void instance_error(Instance* inst, Source_Pos pos, const char* fmt, ...);
NAPI void instance_fatal_error(Instance* inst, Source_Pos pos, const char* fmt, ...);
NAPI void instance_fatal_error_note(Instance* inst, Source_Pos pos, const char* fmt, ...);

}
