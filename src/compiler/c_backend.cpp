#include "c_backend.h"

#include "atom.h"
#include "const_resolver.h"
#include "instance.h"
#include "source_pos.h"
#include "ssa.h"
#include "token.h"
#include "type.h"

#include <cassert>
#include <filesystem.h>
#include <logger.h>
#include <memory/allocator.h>
#include <memory/arena.h>
#include <nstring.h>
#include <platform.h>
#include <string_builder.h>

#if NPLATFORM_WINDOWS
#include "microsoft_craziness.h"
typedef MC_Find_Result Windows_SDK_Info;
#endif // NPLATFORM_WINDOWS

namespace Novo {

struct C_Backend {

    Instance* inst;

#if NPLATFORM_WINDOWS
    Windows_SDK_Info wsdk_info;
#endif // NPLATFORM_WINDOWS

    String_Ref clang_path;
};

bool c_backend_emit(Instance* inst)
{
    C_Backend cb_data;
    cb_data.inst = inst;

    Temp_Arena tarena = temp_arena(nullptr);
    Allocator ta = arena_allocator_create(tarena.arena);

#if NPLATFORM_WINDOWS
    cb_data.wsdk_info = find_visual_studio_and_windows_sdk();

    // cb_data.wsdk_info.windows_sdk_version = 10;
    // cb_data.wsdk_info.windows_sdk_root =  (wchar_t*)L"C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0";
    // cb_data.wsdk_info.windows_sdk_um_library_path =  (wchar_t*)L"C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0\\um\\x64";
    // cb_data.wsdk_info.windows_sdk_ucrt_library_path =  (wchar_t*)L"C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0\\ucrt\\x64";
    // cb_data.wsdk_info.vs_exe_path =  (wchar_t*)L"C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.37.32822\\bin\\Hostx64\\x64";
    // cb_data.wsdk_info.vs_tools_path =  (wchar_t*)L"C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools";
    // cb_data.wsdk_info.vs_library_path =  (wchar_t*)L"C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.37.32822\\lib\\x64";

    // log_warn("windows_sdk_version: %i", cb_data.wsdk_info.windows_sdk_version);
    // log_warn("windows_sdk_root: %s", wide_string_to_regular(__some_allocator___, cb_data.wsdk_info.windows_sdk_root).data);
    // log_warn("windows_sdk_um_library_path: %s", wide_string_to_regular(__some_allocator___, cb_data.wsdk_info.windows_sdk_um_library_path).data);
    // log_warn("windows_sdk_ucrt_library_path: %s", wide_string_to_regular(__some_allocator___, cb_data.wsdk_info.windows_sdk_ucrt_library_path).data);
    // log_warn("vs_exe_path: %s", wide_string_to_regular(__some_allocator___, cb_data.wsdk_info.vs_exe_path).data);
    // log_warn("vs_tools_path: %s", wide_string_to_regular(__some_allocator___, cb_data.wsdk_info.vs_tools_path).data);
    // log_warn("vs_library_path: %s", wide_string_to_regular(__some_allocator___, cb_data.wsdk_info.vs_library_path).data);

    assert(cb_data.wsdk_info.windows_sdk_version);

    String_Ref tools_path = wide_string_to_regular(&ta, cb_data.wsdk_info.vs_tools_path);

    cb_data.clang_path = string_format(&ta, "%.*s\\Llvm\\x64\\bin\\clang.exe",
                            (int)tools_path.length, tools_path.data);
    assert(fs_is_file(cb_data.clang_path));

#else
    cb_data.clang_path = "clang";
#endif // NPLATFORM_WINDOWS

    C_Backend* cb = &cb_data;

    SSA_Program* program = &inst->ssa_program;

    String_Builder sb;
    string_builder_init(&sb, &ta);

    const char* preamble =
R"PREAMBLE(typedef unsigned char       u8;
typedef unsigned short     u16;
typedef unsigned int       u32;
typedef unsigned long long u64;

typedef signed char       s8;
typedef signed short     s16;
typedef signed int       s32;
typedef signed long long s64;

typedef float  r32;
typedef double r64;

typedef u64 p_uint_t;

#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>

#define novo_c_assert(cond, msg_ptr, file, line, col) { \
    if (!cond) { \
        fprintf(stderr, "%s:%u:%u: Assertion failed", file, line, col); \
        if (msg_ptr) fprintf(stderr, ": \"%.*s\"\n", (int)msg_ptr->length, msg_ptr->data); \
        else fprintf(stderr, "!\n"); \
        raise(SIGABRT); \
    }  \
}

)PREAMBLE";

    string_builder_append(&sb, "/* Preamble */\n");
    string_builder_append(&sb, "%s\n", preamble);
    string_builder_append(&sb, "/* End preamble */\n\n");

    // Type declarations
    string_builder_append(&sb, "/* Type declarations */\n");
    for (s64 i = 0; i < inst->enum_types.count; i++) {
        c_backend_emit_enum_declaration(cb, &sb, inst->enum_types[i]);
        string_builder_append(&sb, "\n");
    }
    for (s64 i = 0; i < inst->struct_types.count; i++) {
        c_backend_emit_struct_declaration(cb, &sb, inst->struct_types[i]);
        string_builder_append(&sb, "\n");
    }
    string_builder_append(&sb, "/* End type declarations */\n\n");

    // Constants
    string_builder_append(&sb, "/* Constants */\n");
    for (s64 i = 0; i < inst->ssa_program.constants.count; i++) {

        SSA_Constant const_info = inst->ssa_program.constants[i];

        if (const_info.from_expression == nullptr) {
            continue;
        }

        char cname[32];
        string_format(cname, "const c%lld", const_info.offset);
        c_backend_emit_c_type(cb, &sb, const_info.type, cname);

        string_builder_append(&sb, " = ");

        c_backend_emit_constant_expression(cb, &sb, const_info.from_expression);

        string_builder_append(&sb, ";\n");
    }
    string_builder_append(&sb, "/* End constants*/\n\n");

    // Globals
    string_builder_append(&sb, "/* Global declarations */\n");
    for (s64 i = 0; i < inst->ssa_program.globals.count; i++) {
        SSA_Global* global = &inst->ssa_program.globals[i];

        char cname[32];
        string_format(cname, "g%u", i);
        c_backend_emit_c_type(cb, &sb, global->type, cname);

        string_builder_append(&sb, " = ");

        if (global->type->kind == Type_Kind::STRUCT) {
            SSA_Constant const_info = inst->ssa_program.constants[global->initializer_constant_index];
            c_backend_emit_constant_expression(cb, &sb, const_info.from_expression);
        } else {
            string_builder_append(&sb, "c%u", inst->ssa_program.constants[global->initializer_constant_index].offset);
        }
        string_builder_append(&sb, ";\n");
    }
    string_builder_append(&sb, "/* End global declarations */\n\n");

    // Function declarations
    string_builder_append(&sb, "/* Function declarations */\n");
    for (s64 i = 0; i < program->functions.count; i++) {

        SSA_Function* func = &program->functions[i];

        if (func->run_wrapper) continue;

        c_backend_emit_function_decl(cb, &sb, func);
        string_builder_append(&sb, ";\n");
    }
    string_builder_append(&sb, "/* End function declarations */\n\n");

    Stack<u32> arg_stack;
    stack_init(&ta, &arg_stack, 32);

    // Function definitions
    string_builder_append(&sb, "/* Function definitions */\n");
    s64 emitted_function_count = 0;
    for (s64 i = 0; i < program->functions.count; i++) {

        assert(stack_count(&arg_stack) == 0);

        SSA_Function *func = &program->functions[i];
        if (func->foreign || func->run_wrapper) continue;

        if (emitted_function_count++ > 0) string_builder_append(&sb, "\n");

        c_backend_emit_function_decl(cb, &sb, func);
        string_builder_append(&sb, "\n");
        c_backend_emit_function_body(cb, &sb, i, &arg_stack);
    }
    string_builder_append(&sb, "/* End function definitions */\n\n");

    const char* postamble =
R"POSTAMBLE(int main(int argc, char** argv) {
    return novo_main();
})POSTAMBLE";

    string_builder_append(&sb, "/* Postamble */\n");
    string_builder_append(&sb, "%s\n", postamble);
    string_builder_append(&sb, "/* End postamble */\n");

    String c_str = string_builder_to_string(&sb);

    assert(inst->options.output);

    String c_filename = string_format(&ta, "%s_cback.c", inst->options.output);
    fs_write_entire_file(c_filename, c_str);

#if NPLATFORM_WINDOWS
    String_Ref link_flags = "-lmsvcrtd -Wl,-nodefaultlib:libcmt,-nodefaultlib:libcmtd,-nodefaultlib:libmsvcrt";
#else
    String_Ref link_flags = "";
#endif // NPLATFORM_WINDOWS

    String_Ref commands[] = { cb->clang_path, "-std=c99", "-g",
                          "-Wno-incompatible-library-redeclaration", "-Wno-format-security",
                          c_filename, "-o", inst->options.output,
                          inst->support_lib_s_path,
                          link_flags,
                        };

    Command_Result c_res = platform_run_command(commands, tarena.arena);

    if (!c_res.success) {
        log_error("C backend errors:\n%s", c_res.error_string.data);
    } else if (inst->options.verbose && c_res.error_string.length) {
        log_warn("C backend warnings:\n%s", c_res.error_string.data);
    }

    if (inst->options.verbose && c_res.result_string.length) {
        String_Ref str = c_res.result_string;
        if (str[str.length - 1] == '\n') str.length -= 1;
        log_info("C backend: %.*s", (int) str.length, str.data);
    }

    if (c_res.success && !inst->options.keep_c_backend_output) {
        fs_remove(c_filename);
    }

    temp_arena_release(tarena);

#if NPLATFORM_WINDOWS
    free_resources(&cb->wsdk_info);
#endif // NPLATFORM_WINDOWS

    return c_res.success;
}

void c_backend_emit_struct_declaration(C_Backend* cb, String_Builder* sb, Type *type)
{
    assert(type->kind == Type_Kind::STRUCT);

    String name = atom_string(type->structure.name);

    string_builder_append(sb, "typedef struct %.*s\n", (int)name.length, name.data);
    string_builder_append(sb, "{\n");

    for (s64 i = 0; i < type->structure.members.count; i++) {
        Type_Struct_Member_Resolved member = type->structure.members[i];

        string_builder_append(sb, "    ");
        String_Ref member_name = atom_string(member.name);
        c_backend_emit_c_type(cb, sb, member.type, member_name);
        string_builder_append(sb, ";\n");
    }

    string_builder_append(sb, "} %.*s;\n", (int)name.length, name.data);
}

void c_backend_emit_enum_declaration(C_Backend* cb, String_Builder* sb, Type *type)
{
    assert(type->kind == Type_Kind::ENUM);

    String name = atom_string(type->enumeration.name);
    Type* strict_type = type->enumeration.strict_type;

    string_builder_append(sb, "typedef enum %.*s : ", (int)name.length, name.data);
    c_backend_emit_c_type(cb, sb, strict_type, "");
    string_builder_append(sb, "\n{\n");

    for (s64 i = 0; i < type->enumeration.members.count; i++) {
        Type_Enum_Member member = type->enumeration.members[i];

        string_builder_append(sb, "    ");
        String_Ref member_name = atom_string(member.name);
        string_builder_append(sb, "%.*s_%.*s = 0x%x", (int)name.length, name.data, (int)member_name.length, member_name.data, member.value);


#if NOVO_C_BACKEND_PRINT_SSA_COMMENTS
        const char* fmt = strict_type->integer.sign ? ", // %llu\n" : ", // %lld\n";
        string_builder_append(sb, fmt, member.value);
#else // NOVO_C_BACKEND_PRINT_SSA_COMMENTS
        string_builder_append(sb, ",\n");
#endif // NOVO_C_BACKEND_PRINT_SSA_COMMENTS

    }


    string_builder_append(sb, "} %.*s;\n", (int)name.length, name.data);
}

void c_backend_emit_c_type(C_Backend* cb, String_Builder* sb, Type* type, String_Ref name, CType_Flags flags/* =CTYPE_FLAG_NONE*/)
{
    String result = c_backend_emit_c_type(cb, type, name, flags);

    string_builder_append(sb, "%.*s", (int)result.length, result.data);
}

String c_backend_emit_c_type(C_Backend* cb, Type* type, String_Ref name, CType_Flags flags/* =CTYPE_FLAG_NONE*/)
{
    if (string_equal(name, "main")) {
        assert(type->kind == Type_Kind::FUNCTION);

        name = "novo_main";
    }

    Temp_Arena tarena = temp_arena(nullptr);
    Allocator ta = arena_allocator_create(tarena.arena);

    String result = {};

    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;

        case Type_Kind::VOID: {
            result = string_format(&ta, "void %.*s", (int)name.length, name.data);
            break;
        }

        case Type_Kind::INTEGER: {
            result = string_format(&ta,
                                   name.length ? "%c%ld %.*s" : "%c%ld%.*s",
                                   type->integer.sign ? 's' : 'u',
                                   type->bit_size,
                                   (int)name.length, name.data);
            break;
        }

        case Type_Kind::BOOLEAN: {
            result = string_format(&ta, "bool %.*s", (int)name.length, name.data);
            break;
        }

        case Type_Kind::POINTER: {
            if (type == cb->inst->builtin_type_cstring) {
                result = string_format(&ta, "const char (*%.*s)", (int)name.length, name.data);
            } else {
                if (name.length) result = c_backend_emit_c_type(cb, type->pointer.base, string_format(&ta, "(*%.*s)", (int)name.length, name.data), flags);
                else             result = c_backend_emit_c_type(cb, type->pointer.base, "(*)", flags);
            }
            break;
        }

        case Type_Kind::ARRAY: {

            if (flags & CTYPE_FLAG_ARRAY_ELEM_POINTER) {
                Type* ptr_type = pointer_type_get(cb->inst, type->array.element_type);
                result = c_backend_emit_c_type(cb, ptr_type, name, flags);
            } else {
                if (name.length) result = c_backend_emit_c_type(cb, type->array.element_type, string_format(&ta, "%.*s[%lld]", (int)name.length, name.data, type->array.length), flags);
                else             result = c_backend_emit_c_type(cb, type->array.element_type, string_format(&ta, "[%lld]", type->array.length), flags);
            }
            break;
        }

        case Type_Kind::FUNCTION: {
            String_Builder sb;
            string_builder_init(&sb, &ta);

            string_builder_append(&sb, "(%s%.*s)(", (type->flags & TYPE_FLAG_FOREIGN_VARARG) ? "*" : "",
                                  (int)name.length, name.data);

            if (type->function.param_types.count) {
                for (s64 i = 0; i < type->function.param_types.count; i++) {
                    string_builder_append(&sb, "%s%s", i == 0 ? "" : ", ", c_backend_emit_c_type(cb, type->function.param_types[i], "", flags));
                }
            }

            if (type->flags & TYPE_FLAG_FOREIGN_VARARG) {
                if (type->function.param_types.count) string_builder_append(&sb, ", ");
                string_builder_append(&sb, "...");
            }

            string_builder_append(&sb, ")");

            String result = string_builder_to_string(&sb);

            Type* return_type = type->function.return_type;
            if (return_type->kind == Type_Kind::ARRAY) {
                return_type = pointer_type_get(cb->inst, return_type->array.element_type);
            }

            return c_backend_emit_c_type(cb, return_type, result, flags);
        }

        case Type_Kind::STRUCT: {
            String_Builder sb;
            string_builder_init(&sb, &ta);

            String struct_name = atom_string(type->structure.name);

            string_builder_append(&sb, "%.*s", (int)struct_name.length, struct_name.data);

            if (name.length) string_builder_append(&sb, " %.*s", (int)name.length, name.data);

            result = string_builder_to_string(&sb);
            break;
        }

        case Type_Kind::ENUM: {
            String_Builder sb;
            string_builder_init(&sb, &ta);

            String enum_name = atom_string(type->enumeration.name);

            string_builder_append(&sb, "%.*s", (int)enum_name.length, enum_name.data);

            if (name.length) string_builder_append(&sb, " %.*s", (int)name.length, name.data);

            result = string_builder_to_string(&sb);
            break;
        }
    }

    assert(result.length);
    return result;
}

void c_backend_emit_function_decl(C_Backend* cb, String_Builder* sb, SSA_Function* func)
{
    Temp_Arena tarena = temp_arena((Arena*)sb->allocator->user_data);
    Allocator ta = arena_allocator_create(tarena.arena);

    String_Builder local_sb;
    string_builder_init(&local_sb, &ta);

    String_Ref name = atom_string(func->name);
    if (string_equal(name, "main")) {
        name = "novo_main";
    }

    Type* return_type = func->type->function.return_type;
    Type* c_ret_type = return_type;

    string_builder_append(&local_sb, "%.*s(", (int)name.length, name.data);

    Type* sret_type = nullptr;
    if (return_type->kind == Type_Kind::STRUCT) {
        c_ret_type = cb->inst->builtin_type_void;
        sret_type = pointer_type_get(cb->inst, return_type);
    } else if (return_type->kind == Type_Kind::ARRAY) {
        c_ret_type = cb->inst->builtin_type_void;
        sret_type = pointer_type_get(cb->inst, return_type->array.element_type);
    }

    if (sret_type) {
        c_backend_emit_c_type(cb, &local_sb, sret_type, "sret");
        if (func->param_count > 1) {
            string_builder_append(&local_sb, ", ");
        }
    }

    if (func->param_count) {
        for (s64 i = 0; i < func->type->function.param_types.count; i++) {
            Type* param_type = func->type->function.param_types[i];

            if (i > 0) string_builder_append(&local_sb, ", ");

            char param_name[32];
            string_format(param_name, "p%lld", i);

            Type* orig_type = nullptr;

            if (param_type->kind == Type_Kind::STRUCT) {
                param_type = pointer_type_get(cb->inst, param_type);
            } else if (param_type->kind == Type_Kind::ARRAY || (param_type->kind == Type_Kind::POINTER && param_type->pointer.base->kind == Type_Kind::ARRAY)) {
                orig_type = param_type;
            }

            c_backend_emit_c_type(cb, &local_sb, param_type, param_name, CTYPE_FLAG_ARRAY_ELEM_POINTER);

            if (orig_type) {
                String tname = temp_type_string(cb->inst, orig_type);
                string_builder_append(&local_sb, "/* %.*s */", (int)tname.length, tname.data);
            }
        }
    }

    if (func->type->flags & TYPE_FLAG_FOREIGN_VARARG) {
        string_builder_append(&local_sb, ", ...");
    }

    string_builder_append(&local_sb, ")");

    String result = string_builder_to_string(&local_sb);
    c_backend_emit_c_type(cb, sb, c_ret_type, result, CTYPE_FLAG_ARRAY_ELEM_POINTER);

    temp_arena_release(tarena);
}

template <typename T>
static T _fetch_(SSA_Block* block, s64* offset) {
    T result = *(T*)&block->bytes[*offset];
    *offset += sizeof(T);
    return result;
}

void c_backend_emit_function_body(C_Backend* cb, String_Builder* sb, u32 fn_index, Stack<u32> *arg_stack)
{
    assert(fn_index < cb->inst->ssa_program.functions.count);
    SSA_Function* func = &cb->inst->ssa_program.functions[fn_index];

    Temp_Arena tarena = temp_arena((Arena*)sb->allocator->user_data);
    Allocator ta = arena_allocator_create(tarena.arena);

    string_builder_append(sb, "{\n");

    char reg_name[32];

    for (s64 i = 0; i < func->allocs.count; i++) {
        string_builder_append(sb, "    ");
        string_format(reg_name, "alloc_r%u", i);

        SSA_Register_Handle reg = func->allocs[i].alloc_reg;

        c_backend_emit_c_type(cb, sb, func->registers[reg.index].type, reg_name);
        string_builder_append(sb, ";\n");
    }

    s64 register_count = func->registers.count;

    for (s64 i = 0; i < register_count; i++) {

        if (!func->registers[i].used) continue;

        Type* register_type = func->registers[i].type;

        string_builder_append(sb, "    ");
        string_format(reg_name, "r%u", i);

        Type* initial_type = register_type;
        bool is_struct = register_type->kind == Type_Kind::STRUCT;
        bool is_array = register_type->kind == Type_Kind::ARRAY;

        if (is_array) {
            register_type = pointer_type_get(cb->inst, register_type->array.element_type);
        } else if (is_struct || func->registers[i].alloc_reg) {
            register_type = pointer_type_get(cb->inst, register_type);
        }

        c_backend_emit_c_type(cb, sb, register_type, reg_name, CTYPE_FLAG_ARRAY_ELEM_POINTER);

        string_builder_append(sb, ";");
        if (is_array || is_struct) {
            String tname = temp_type_string(cb->inst, initial_type);
            string_builder_append(sb, " /* %.*s */", (int)tname.length, tname.data);
        }
        string_builder_append(sb, "\n");
    }

    string_builder_append(sb, "\n");

    s64 bi = 0;
    while (bi >= 0 && bi < func->blocks.count) {
        SSA_Block* block = &func->blocks[bi];

        if (block->bytes.count == 0) {
            bi = block->next_index;
            continue;
        }

        if (bi > 0) {
            String_Ref block_name = c_backend_block_name(cb, func, bi, tarena.arena);
            string_builder_append(sb, "%.*s:\n", (int)block_name.length, block_name.data);
        }

#define FETCH() (_fetch_<u8>(block, &instruction_offset))
#define FETCH16() (_fetch_<u16>(block, &instruction_offset))
#define FETCH32() (_fetch_<u32>(block, &instruction_offset))
#define FETCH64() (_fetch_<u64>(block, &instruction_offset))

        s64 instruction_offset = 0;
        while (instruction_offset < block->bytes.count) {

            bool no_op = false;
            bool no_print = false;
#if NOVO_C_BACKEND_PRINT_SSA_COMMENTS
            s64 ip = instruction_offset;
#endif // NOVO_C_BACKEND_PRINT_SSA_COMMENTS

            SSA_Op op = (SSA_Op)FETCH();

            switch (op) {
                case SSA_OP_NOP: assert(false); break;

#define BINOP(op) case SSA_OP_##op: { \
    /*u8 size =*/ FETCH(); \
    u32 result_reg = FETCH32(); \
    u32 left_reg = FETCH32(); \
    u32 right_reg = FETCH32(); \
    String_Ref op_string = tmp_token_kind_str(TOK_##op); \
    string_builder_append(sb, "    r%u = r%u %.*s r%u;", result_reg, left_reg, (int)op_string.length, op_string.data, right_reg); \
    break; \
}
                BINOP(ADD)
                BINOP(SUB)
                BINOP(MUL)
                BINOP(DIV)


#define CMP_BINOP(op) case SSA_OP_##op: { \
    /*u8 size =*/ FETCH(); \
    u32 result_reg = FETCH32(); \
    u32 left_reg = FETCH32(); \
    u32 right_reg = FETCH32(); \
    String_Ref op_string = tmp_token_kind_str(TOK_##op); \
    string_builder_append(sb, "    r%u = r%u %.*s r%u;", result_reg, left_reg, (int)op_string.length, op_string.data, right_reg); \
    break; \
}

                CMP_BINOP(LT);
                CMP_BINOP(GT);
                CMP_BINOP(EQ);
                CMP_BINOP(NEQ);
                CMP_BINOP(LTEQ);
                CMP_BINOP(GTEQ);

                case SSA_OP_BITCAST: {
                    u32 result_reg = FETCH32();
                    u32 operand_reg = FETCH32();

                    Type* target_type = func->registers[result_reg].type;
                    assert(target_type->kind == Type_Kind::POINTER);

                    string_builder_append(sb, "    r%u = (", result_reg);
                    c_backend_emit_c_type(cb, sb, target_type, "");
                    string_builder_append(sb, ")r%u;", operand_reg);
                    break;
                }

                case SSA_OP_TRUNC: {
                    /*u8 size =*/ FETCH();
                    u32 result_reg = FETCH32();
                    u32 operand_reg = FETCH32();

                    string_builder_append(sb, "    r%u = (", result_reg);
                    c_backend_emit_c_type(cb, sb, func->registers[result_reg].type, "");
                    string_builder_append(sb, ")r%u;", operand_reg);
                    break;
                }

                case SSA_OP_SEXT: {
                    /*u8 dest_size =*/ FETCH();
                    /*u8 src_size =*/ FETCH();
                    u32 result_reg = FETCH32();
                    u32 operand_reg = FETCH32();

                    string_builder_append(sb, "    r%u = (", result_reg);
                    c_backend_emit_c_type(cb, sb, func->registers[result_reg].type, "");
                    string_builder_append(sb, ")r%u;", operand_reg);
                    break;
                }

                case SSA_OP_ZEXT: {
                    /*u8 dest_size =*/ FETCH();
                    u32 result_reg = FETCH32();
                    u32 operand_reg = FETCH32();

                    string_builder_append(sb, "    r%u = (", result_reg);
                    c_backend_emit_c_type(cb, sb, func->registers[result_reg].type, "");
                    string_builder_append(sb, ")r%u;", operand_reg);
                    break;
                }


                case SSA_OP_ALLOC: {
                    u32 result_reg = FETCH32();
                    /*u64 size =*/ FETCH64();

                    string_format(reg_name, "alloc_r%u", result_reg);
                    string_builder_append(sb, "    r%u = (", result_reg);
                    Type *ctype = func->registers[result_reg].type;
                    if (ctype->kind == Type_Kind::ARRAY) {
                        ctype = pointer_type_get(cb->inst, ctype->array.element_type);
                    } else {
                        ctype = pointer_type_get(cb->inst, ctype);
                    }
                    c_backend_emit_c_type(cb, sb, ctype, "", CTYPE_FLAG_ARRAY_ELEM_POINTER);
                    string_builder_append(sb, ")&(%s);", reg_name);
                    break;
                }

                case SSA_OP_GLOB_PTR: {
                    u32 result_reg = FETCH32();
                    u32 glob_idx = FETCH32();

                    string_builder_append(sb, "    r%u = &(g%u);", result_reg, glob_idx);
                    break;
                }

                case SSA_OP_MEMCPY: {
                    u32 dest_ptr_reg = FETCH32();
                    u32 source_ptr_reg = FETCH32();
                    u64 byte_count = FETCH64();

                    string_format(reg_name, "r%u", dest_ptr_reg);
                    string_builder_append(sb, "    memcpy(%s, ", reg_name);
                    string_format(reg_name, "r%u", source_ptr_reg);
                    string_builder_append(sb, "%s, %llu);", reg_name, byte_count);
                    break;
                }

                case SSA_OP_STORE_PTR: {
                    /*u8 size =*/ FETCH();
                    u32 ptr_reg = FETCH32();
                    u32 value_reg = FETCH32();

                    SSA_Register ptr_reg_data = func->registers[ptr_reg];
                    Type* ptr_type = ptr_reg_data.type;

                    if (ptr_reg_data.alloc_reg) {
                        ptr_type = pointer_type_get(cb->inst, ptr_type);
                    }

                    string_builder_append(sb, "    *(r%u) = (", ptr_reg);
                    c_backend_emit_c_type(cb, sb, ptr_type->pointer.base, "", CTYPE_FLAG_ARRAY_ELEM_POINTER);
                    string_builder_append(sb, ")r%u;",  value_reg);
                    break;
                }

                case SSA_OP_LOAD_IM: {
                    u8 size = FETCH();
                    u32 result_reg = FETCH32();

                    u64 value;
                    switch (size) {
                        default: assert(false);
                        case 1: value = FETCH(); break;
                        case 2: value = FETCH16(); break;
                        case 4: value = FETCH32(); break;
                        case 8: value = FETCH64(); break;
                    }

                    string_builder_append(sb, "    r%u = 0x%x;", result_reg, value);
                    break;
                }

                case SSA_OP_LOAD_PARAM: {
                    u32 result_reg = FETCH32();
                    u32 param_index = FETCH32();

                    bool sret = false;
                    if (func->sret) {
                        if (param_index == 0) {
                            // Loading return value
                            sret = true;
                        } else {
                            // Skip return value
                            param_index--;
                        }
                    }

                    string_builder_append(sb, "    r%u = ", result_reg);

                    if (sret) {
                        string_builder_append(sb, "sret;");
                    } else {
                        string_builder_append(sb, "p%u;", param_index);
                    }

                    break;
                }

                case SSA_OP_LOAD_PTR: {
                    /*u8 size =*/ FETCH();
                    u32 result_reg = FETCH32();
                    u32 ptr_reg = FETCH32();

                    string_builder_append(sb, "    r%u = *(r%u);", result_reg, ptr_reg);
                    break;
                }

                case SSA_OP_LOAD_CONST: {
                    u32 result_reg = FETCH32();
                    u32 offset = FETCH32();

                    Type* register_type = func->registers[result_reg].type;
                    Type* pointer_type = nullptr;
                    if (register_type->kind == Type_Kind::ARRAY) {
                        pointer_type = pointer_type_get(cb->inst, register_type->array.element_type);
                    } else {
                        pointer_type = pointer_type_get(cb->inst, register_type);
                    }
                    assert(pointer_type);

                    string_builder_append(sb, "    r%u = (", result_reg);
                    c_backend_emit_c_type(cb, sb, pointer_type, "");
                    string_builder_append(sb, ")&c%lld;", offset);

                    break;
                }

                case SSA_OP_LOAD_ENUM: {
                    u32 result_reg = FETCH32();
                    u64 index = FETCH64();

                    Type* enum_type = func->registers[result_reg].type;
                    assert(enum_type->kind == Type_Kind::ENUM);

                    u64 value;
                    switch (enum_type->bit_size) {
                        default: assert(false && "Invalid size in SSA_OP_LOAD_ENUM");

                        case 8: value = FETCH(); break;
                        case 16: value = FETCH16(); break;
                        case 32: value = FETCH32(); break;
                        case 64: value = FETCH64(); break;
                    }

                    assert(value == (u64)enum_type->enumeration.members[index].value);

                    String ename = atom_string(enum_type->enumeration.name);
                    String emember_name = atom_string(enum_type->enumeration.members[index].name);

                    string_builder_append(sb, "    r%u = %.*s_%.*s;", result_reg, (int)ename.length, ename.data, (int)emember_name.length, emember_name.data);
                    break;
                }

                case SSA_OP_STRUCT_OFFSET: {
                    u32 result_reg = FETCH32();
                    u32 base_ptr_reg = FETCH32();
                    u32 offset = FETCH32();
                    u16 index = FETCH16();

                    Type* struct_type = func->registers[base_ptr_reg].type;
                    if (struct_type->kind == Type_Kind::POINTER) {
                        struct_type = struct_type->pointer.base;
                    }
                    assert(struct_type->kind == Type_Kind::STRUCT);

                    Type_Struct_Member_Resolved member = struct_type->structure.members[index];
                    assert(member.offset / 8 == offset);
                    String_Ref member_name = atom_string(member.name);

                    string_builder_append(sb, "    r%u = &(r%u)->%.*s;", result_reg, base_ptr_reg, (int)member_name.length, member_name.data);
                    break;
                }

                case SSA_OP_POINTER_OFFSET: {
                    u64 size = FETCH64();
                    u32 result_reg = FETCH32();
                    u32 base_ptr_reg = FETCH32();
                    u32 index_reg = FETCH32();

                    Type* base_ptr_type = func->registers[base_ptr_reg].type;

                    Type* base_pointee_type = nullptr;
                    if (base_ptr_type->kind == Type_Kind::POINTER) {
                        base_pointee_type = base_ptr_type->pointer.base;
                    } else if (base_ptr_type->kind == Type_Kind::ARRAY) {
                        base_pointee_type = base_ptr_type->array.element_type;
                        base_ptr_type = pointer_type_get(cb->inst, base_pointee_type);
                    } else assert(false);
                    assert(base_pointee_type->bit_size / 8 == size);

                    assert(func->registers[result_reg].type == base_ptr_type);

                    string_builder_append(sb, "    r%u = (", result_reg);
                    c_backend_emit_c_type(cb, sb, base_ptr_type, "");
                    string_builder_append(sb, ")r%u + r%u;", base_ptr_reg, index_reg);

                    break;
                }

                case SSA_OP_POINTER_DIFF: {
                    u64 size = FETCH64();
                    u32 result_reg = FETCH32();
                    u32 left_reg = FETCH32();
                    u32 right_reg = FETCH32();

                    Type* pointer_type = func->registers[left_reg].type;
                    assert(pointer_type->kind == Type_Kind::POINTER);

                    assert(pointer_type == func->registers[right_reg].type);
                    assert(pointer_type->pointer.base->bit_size / 8 == size);

                    string_builder_append(sb, "    r%u = r%u - r%u;", result_reg, left_reg, right_reg);
                    break;
                }

                case SSA_OP_PUSH: {
                    no_print = true;
                    u32 arg_reg = FETCH32();
                    stack_push(arg_stack, arg_reg);
                    break;
                }

                case SSA_OP_POP_N: {
                    no_print = true;
                    u32 count = FETCH32();
                    stack_pop(arg_stack, count);
                    break;
                }

                case SSA_OP_CALL: {
                    u32 result_reg = FETCH32();
                    u32 fn_index = FETCH32();

                    SSA_Function* callee = &cb->inst->ssa_program.functions[fn_index];
                    String callee_name = atom_string(callee->name);

                    assert(!callee->foreign);

                    u32 arg_count = callee->param_count;
                    assert(stack_count(arg_stack) >= arg_count);

                    bool return_used = func->registers[result_reg].used;

                    string_builder_append(sb, "    ");
                    if (return_used) {
                        string_builder_append(sb, "r%u = ", result_reg);
                    }

                    string_builder_append(sb, "%.*s(", (int)callee_name.length, callee_name.data);

                    s64 arg_offset = arg_count - 1;
                    for (s64 i = 0; i < arg_count; i++) {

                        if (i > 0) string_builder_append(sb, ", ");

                        u32 arg_reg = stack_peek(arg_stack, arg_offset--);
                        // Type* arg_type = func->registers[arg_reg].type;

                        string_builder_append(sb, "r%u", arg_reg);
                    }

                    string_builder_append(sb, ");");
                    break;
                }

                case SSA_OP_CALL_FOREIGN: {
                    u32 result_reg = FETCH32();
                    u32 fn_index = FETCH32();
                    u16 arg_count = FETCH16();

                    SSA_Function* callee = &cb->inst->ssa_program.functions[fn_index];
                    assert(callee->foreign);

                    String callee_name = atom_string(callee->name);

                    assert(stack_count(arg_stack) >= arg_count);

                    bool return_used = false;
                    if (callee->sret) {
                        u32 res_reg = stack_peek(arg_stack, arg_count - 1);
                        if(func->registers[res_reg].used) {
                            return_used = true;
                            string_builder_append(sb, "    *(r%u) = ", res_reg);
                        }
                    } else if (func->registers[result_reg].used) {
                        return_used = true;
                        string_builder_append(sb, "    r%u = ", result_reg);
                    }

                    if (!return_used) {
                        string_builder_append(sb, "    ");
                    }
                    string_builder_append(sb, "%.*s(", (int)callee_name.length, callee_name.data);

                    if (callee->sret) {
                        arg_count -= 1;
                    }

                    s64 arg_offset = arg_count - 1;
                    for (s64 i = 0; i < arg_count; i++) {

                        if (i > 0) string_builder_append(sb, ", ");

                        u32 arg_reg = stack_peek(arg_stack, arg_offset--);

                        Type* arg_type = nullptr;
                        if ((callee->type->flags & TYPE_FLAG_FOREIGN_VARARG) && i >= callee->type->function.param_types.count) {
                            arg_type = func->registers[arg_reg].type;
                        } else {
                            arg_type = callee->type->function.param_types[i];
                        }
                        assert(arg_type);

                        if (arg_type == cb->inst->builtin_type_cstring) {
                            string_builder_append(sb, "(const char*) ");
                        }
                        string_builder_append(sb, "r%u", arg_reg);
                    }

                    string_builder_append(sb, ");");

                    break;
                }

                case SSA_OP_RET: {
                    u32 value_reg = FETCH32();

                    if (func->sret) {
                        string_builder_append(sb, "    return ;");
                    } else {
                        string_builder_append(sb, "    return r%u;", value_reg);
                    }
                    break;
                }

                case SSA_OP_RET_VOID: {
                    string_builder_append(sb, "    return;");
                    break;
                }

                case SSA_OP_JMP_IF: {
                    u32 cond_reg = FETCH32();
                    u32 true_block = FETCH32();
                    u32 false_block = FETCH32();

                    String_Ref tname = c_backend_block_name(cb, func, true_block, tarena.arena);
                    String_Ref fname = c_backend_block_name(cb, func, false_block, tarena.arena);

                    string_builder_append(sb, "    if (r%u) goto %.*s;\n", cond_reg, (int)tname.length, tname.data);
                    string_builder_append(sb, "    else goto %.*s;", (int)fname.length, fname.data);

                    break;
                }

                case SSA_OP_JMP: {
                    u32 target_block = FETCH32();

                    String_Ref tname = c_backend_block_name(cb, func, target_block, tarena.arena);

                    string_builder_append(sb, "    goto %.*s;", (int)tname.length, tname.data);
                    break;
                }

                case SSA_OP_ASSERT: {
                    u32 offset = instruction_offset - sizeof(SSA_Op);
                    u32 cond_reg = FETCH32();
                    u32 string_reg = FETCH32();

                    Source_Pos pos;
                    bool found = hash_table_find(&cb->inst->ssa_program.instruction_origin_positions, { offset, fn_index, (u32)bi }, &pos);
                    assert(found);

                    Imported_File file = cb->inst->imported_files[pos.file_index];
                    String file_name = atom_string(file.name);
                    Line_Info li = line_info(file.newline_offsets, pos.offset);

                    String escaped_fname = convert_special_characters_to_escape_characters(&ta, file_name);

                    string_builder_append(sb, "    novo_c_assert(r%u, r%u, \"%.*s\", %u, %u);", cond_reg, string_reg,
                                          (int)escaped_fname.length, escaped_fname.data, li.line, li.offset);
                    break;
                }
            }

#if NOVO_C_BACKEND_PRINT_SSA_COMMENTS
            if (!no_print)  {
                if (no_op) string_builder_append(sb, "    // ");
                else string_builder_append(sb, " // ");
                ssa_print_instruction(cb->inst, sb, &cb->inst->ssa_program, func, ip, block->bytes);
            }
#else // NOVO_C_BACKEND_PRINT_SSA_COMMENTS
            if (!no_print && !no_op) string_builder_append(sb, "\n");
#endif // NOVO_C_BACKEND_PRINT_SSA_COMMENTS

        }

        bi = block->next_index;
    }

    string_builder_append(sb, "}\n");

    temp_arena_release(tarena);

#undef FETCH
#undef FETCH16
#undef FETCH32
#undef FETCH64
#undef BINOP
#undef CMP_BINOP
}

void c_backend_emit_constant_expression(C_Backend* cb, String_Builder* sb, AST_Expression* const_expr)
{
    assert(const_expr->flags & AST_EXPR_FLAG_CONST);

    switch (const_expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;
        case AST_Expression_Kind::IDENTIFIER: assert(false); break;

        case AST_Expression_Kind::UNARY: {
            switch (const_expr->unary.op) {
                default: assert(false); break;

                case '-': {

                    assert(const_expr->resolved_type->kind == Type_Kind::INTEGER);

                    Resolved_Constant rc = const_resolve(cb->inst, const_expr);
                    assert(rc.status == Resolved_Constant_Status::RESOLVED);

                    c_backend_emit_constant_int(cb, sb, const_expr->resolved_type, rc.integer);
                    break;
                };
            }
            break;
        }

        case AST_Expression_Kind::BINARY: assert(false); break;
        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::IMPLICIT_MEMBER: assert(false); break;
        case AST_Expression_Kind::SUBSCRIPT: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::ADDRESS_OF: assert(false); break;
        case AST_Expression_Kind::DEREF: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;

        case AST_Expression_Kind::COMPOUND: {
            if (const_expr->resolved_type->kind == Type_Kind::STRUCT) {
                assert(const_expr->compound.expressions.count == const_expr->resolved_type->structure.members.count);
            } else {
                assert(const_expr->resolved_type->kind == Type_Kind::ARRAY);
                assert(const_expr->compound.expressions.count == const_expr->resolved_type->array.length);
            }

            string_builder_append(sb, "{ ");

            for (s64 i = 0; i < const_expr->compound.expressions.count; i++) {
                if (i > 0) string_builder_append(sb, ", ");

                c_backend_emit_constant_expression(cb, sb, const_expr->compound.expressions[i]);
            }

            string_builder_append(sb, " }");

            break;
        }

        case AST_Expression_Kind::RUN: assert(false); break;
        case AST_Expression_Kind::SIZEOF: assert(false); break;
        case AST_Expression_Kind::ALIGNOF: assert(false); break;
        case AST_Expression_Kind::OFFSETOF: assert(false); break;
        case AST_Expression_Kind::TYPE: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: {
            c_backend_emit_constant_int(cb, sb, const_expr->resolved_type, const_expr->integer_literal);
            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

        case AST_Expression_Kind::STRING_LITERAL: {
            string_builder_append(sb, "{ ");

            String _str = atom_string(const_expr->string_literal);

            Temp_Arena tarena = temp_arena((Arena*)sb->allocator->user_data);
            Allocator ta = arena_allocator_create(tarena.arena);

            String str = convert_special_characters_to_escape_characters(&ta, _str);

            string_builder_append(sb, "(u8*) \"%.*s\"", (int)str.length, str.data);
            string_builder_append(sb, ", %lld }", _str.length);

            temp_arena_release(tarena);
            break;
        }

    }
}

void c_backend_emit_constant_int(C_Backend* cb, String_Builder* sb,  Type* type, u64 value)
{
    assert(type->kind == Type_Kind::INTEGER);

    string_builder_append(sb, "(");
    c_backend_emit_c_type(cb, sb, type, "");
    string_builder_append(sb, ")");

    switch (type->bit_size) {
        default: assert(false);

        case 8: string_builder_append(sb, "0x%hhx", value); break;
        case 16: string_builder_append(sb, "0x%hx", value); break;
        case 32: string_builder_append(sb, "0x%lx", value); break;
        case 64: string_builder_append(sb, "0x%llx", value); break;
    }
}

String_Ref c_backend_block_name(C_Backend* cb, SSA_Function* func, s64 block_index, Arena* arena)
{
    assert(block_index >= 0 && block_index < func->blocks.count);

    Allocator ta = arena_allocator_create(arena);

    String result = string_copy(&ta, atom_string(func->blocks[block_index].name));

    for (s64 i = 0; i < result.length; i++) {
        if (result[i] == '.') result[i] = '_';
    }

    return result;
}

}
