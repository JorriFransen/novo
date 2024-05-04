#include "c_backend.h"

#include "instance.h"
#include "ssa.h"
#include "token.h"
#include "type.h"

// #include <bit>
#include <cassert>
#include <filesystem.h>
#include <logger.h>
#include <nstring.h>
#include <platform.h>
#include <string_builder.h>

namespace Novo {

bool c_backend_emit(Instance* inst)
{
    SSA_Program* program = inst->ssa_program;

    String_Builder sb;
    string_builder_init(&sb, inst->default_allocator);

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
        if (msg_ptr) fprintf(stderr, ": \"%.*s\"\n", (int)msg_ptr->m1, msg_ptr->m0); \
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
    for (s64 i = 0; i < inst->struct_types.count; i++) {
        c_backend_emit_struct_declaration(inst, &sb, inst->struct_types[i]);
        string_builder_append(&sb, "\n");
    }
    string_builder_append(&sb, "/* End type declarations */\n\n");

    // Constants
    string_builder_append(&sb, "/* Constants */\n");
    for (s64 i = 0; i < inst->ssa_program->constants.count; i++) {

        SSA_Constant const_info = inst->ssa_program->constants[i];

        if (const_info.from_expression == nullptr) {
            continue;
        }

        char cname[32];
        string_format(cname, "const c%lld", const_info.offset);
        c_backend_emit_c_type(inst, &sb, const_info.type, cname);

        string_builder_append(&sb, " = ");

        c_backend_emit_constant_expression(inst, &sb, const_info.from_expression);

        string_builder_append(&sb, ";\n");
    }
    string_builder_append(&sb, "/* End constants*/\n\n");

    // Function declarations
    string_builder_append(&sb, "/* Function declarations */\n");
    for (s64 i = 0; i < program->functions.count; i++) {

        SSA_Function* func = &program->functions[i];

        if (func->run_wrapper) continue;

        c_backend_emit_function_decl(inst, &sb, func);
        string_builder_append(&sb, ";\n");
    }
    string_builder_append(&sb, "/* End function declarations */\n\n");

    Stack<u32> arg_stack;
    stack_init(c_allocator(), &arg_stack);


    // Function definitions
    string_builder_append(&sb, "/* Function definitions */\n");
    for (s64 i = 0; i < program->functions.count; i++) {

        assert(stack_count(&arg_stack) == 0);

        if (i > 0) string_builder_append(&sb, "\n");

        SSA_Function *func = &program->functions[i];

        if (func->foreign || func->run_wrapper) continue;

        c_backend_emit_function_decl(inst, &sb, func);
        string_builder_append(&sb, "\n");
        c_backend_emit_function_body(inst, &sb, i, &arg_stack);
    }
    string_builder_append(&sb, "/* End function definitions */\n\n");

    stack_free(&arg_stack);

    const char* postamble =
R"POSTAMBLE(int main(int argc, char** argv) {
    return novo_main();
})POSTAMBLE";

    string_builder_append(&sb, "/* Postamble */\n");
    string_builder_append(&sb, "%s\n", postamble);
    string_builder_append(&sb, "/* End postamble */\n");

    String str = string_builder_to_string(&sb);
    // printf("%.*s\n", (int)str.length, str.data);

    fs_write_entire_file("c_backend_output.c", str);

    Array_Ref<String_Ref> commands({"clang", "-std=c99", "-g", "c_backend_output.c", inst->support_lib_s_path, "-Wno-incompatible-library-redeclaration" });

    Command_Result c_res = platform_run_command(commands, &inst->temp_allocator);

    if (!c_res.success) {
        log_error("C backend errors:\n%s\n", c_res.error_string.data);
    }

    if (c_res.result_string.length) {
        log_info("C backend: %s\n", c_res.result_string.data);
    }

    platform_free_command_result(&c_res);

    return c_res.success;
}

void c_backend_emit_struct_declaration(Instance* inst, String_Builder* sb, Type *type)
{
    assert(type->kind == Type_Kind::STRUCT);

    String name = atom_string(type->structure.name);

    string_builder_append(sb, "typedef struct %.*s\n", (int)name.length, name.data);
    string_builder_append(sb, "{\n");

    char member_name[32];

    for (s64 i = 0; i < type->structure.members.count; i++) {
        Type_Struct_Member member = type->structure.members[i];

        string_builder_append(sb, "    ");
        string_format(member_name, "m%d", i);
        c_backend_emit_c_type(inst, sb, member.type, member_name);
        string_builder_append(sb, ";\n");
    }

    string_builder_append(sb, "} %.*s;\n", (int)name.length, name.data);
}

void c_backend_emit_c_type(Instance* inst, String_Builder* sb, Type* type, String_Ref name)
{
    auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);

    String result = c_backend_emit_c_type(inst, type, name);
    string_builder_append(sb, "%.*s", (int)result.length, result.data);

    temp_allocator_reset(&inst->temp_allocator_data, mark);
}

String c_backend_emit_c_type(Instance* inst, Type* type, String_Ref name)
{
    if (string_equal(name, "main")) {
        assert(type->kind == Type_Kind::FUNCTION);

        name = "novo_main";
    }

    Allocator* ta = &inst->temp_allocator;

    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;

        case Type_Kind::VOID: {
            return string_format(ta, "void %.*s", (int)name.length, name.data);
        }

        case Type_Kind::INTEGER: {
            return string_format(ta,
                                 name.length ? "%c%lld %.*s" : "%c%lld%.*s",
                                 type->integer.sign ? 's' : 'u',
                                 type->bit_size,
                                 (int)name.length, name.data);
        }

        case Type_Kind::BOOLEAN: {
            return string_format(ta, "bool %.*s", (int)name.length, name.data);
        }

        case Type_Kind::POINTER: {
            if (type == inst->builtin_type_cstring) {
                return string_format(ta, "const char (*%.*s)", (int)name.length, name.data);
            } else {
                if (name.length) return c_backend_emit_c_type(inst, type->pointer.base, string_format(ta, "(*%.*s)", (int)name.length, name.data));
                else             return c_backend_emit_c_type(inst, type->pointer.base, "*");
            }
        }

        case Type_Kind::FUNCTION: {
            String_Builder sb;
            string_builder_init(&sb, ta);

            string_builder_append(&sb, "(%s%.*s)(", (type->flags & TYPE_FLAG_FOREIGN_VARARG) ? "*" : "",
                                  (int)name.length, name.data);

            if (type->function.param_types.count) {
                for (s64 i = 0; i < type->function.param_types.count; i++) {
                    string_builder_append(&sb, "%s%s", i == 0 ? "" : ", ", c_backend_emit_c_type(inst, type->function.param_types[i], ""));
                }
            }

            if (type->flags & TYPE_FLAG_FOREIGN_VARARG) {
                if (type->function.param_types.count) string_builder_append(&sb, ", ");
                string_builder_append(&sb, "...");
            }

            string_builder_append(&sb, ")");

            String result = string_builder_to_string(&sb);

            return c_backend_emit_c_type(inst, type->function.return_type, result);
        }

        case Type_Kind::STRUCT: {
            String_Builder sb;
            string_builder_init(&sb, ta);

            String struct_name = atom_string(type->structure.name);

            string_builder_append(&sb, "%.*s", (int)struct_name.length, struct_name.data);

            if (name.length) string_builder_append(&sb, " %.*s", (int)name.length, name.data);

            return string_builder_to_string(&sb);
        }
    }

    assert(false);
    return {};
}

void c_backend_emit_function_decl(Instance* inst, String_Builder* sb, SSA_Function* func)
{
    auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);
    auto ta = &inst->temp_allocator;

    String_Builder local_sb;
    string_builder_init(&local_sb, ta);

    String_Ref name = atom_string(func->name);
    if (string_equal(name, "main")) {
        name = "novo_main";
    }

    string_builder_append(&local_sb, "%.*s(", (int)name.length, name.data);

    if (func->param_count) {
        for (s64 i = 0; i < func->type->function.param_types.count; i++) {
            Type* param_type = func->type->function.param_types[i];

            if (i > 0) string_builder_append(&local_sb, ", ");

            char param_name[32];
            string_format(param_name, "p%lld", i);

            c_backend_emit_c_type(inst, &local_sb, param_type, param_name);
        }
    }

    if (func->type->flags & TYPE_FLAG_FOREIGN_VARARG) {
        string_builder_append(&local_sb, ", ...");
    }

    string_builder_append(&local_sb, ")");

    String result = string_builder_to_string(&local_sb);
    c_backend_emit_c_type(inst, sb, func->type->function.return_type, result);

    temp_allocator_reset(&inst->temp_allocator_data, mark);
}

void c_backend_emit_function_body(Instance* inst, String_Builder* sb, u32 fn_index, Stack<u32> *arg_stack)
{
    assert(fn_index < inst->ssa_program->functions.count);
    SSA_Function* func = &inst->ssa_program->functions[fn_index];

    s64 sret_double_index = -1;

    string_builder_append(sb, "{\n");

    char reg_name[32];

    for (s64 i = 0; i < func->allocs.count; i++) {
        string_builder_append(sb, "    ");
        string_format(reg_name, "alloc_r%u", i);

        SSA_Register_Handle reg = func->allocs[i].alloc_reg;

        c_backend_emit_c_type(inst, sb, func->registers[reg.index].type, reg_name);
        string_builder_append(sb, ";\n");
    }

    s64 register_count = func->registers.count;
    if (func->sret) register_count--; // Used in bytecode to copy struct back to caller, not used in c backend

    for (s64 i = 0; i < register_count; i++) {

        if (!func->registers[i].used) continue;

        Type* register_type = func->registers[i].type;

        string_builder_append(sb, "    ");
        string_format(reg_name, "r%u", i);

        if (register_type->kind == Type_Kind::STRUCT ||
            func->registers[i].alloc_reg) {
            register_type = pointer_type_get(inst, register_type);
        }

        c_backend_emit_c_type(inst, sb, register_type, reg_name);
        string_builder_append(sb, ";\n");
    }

    string_builder_append(sb, "\n");

    s64 bi = 0;
    while (bi >= 0 && bi < func->blocks.count) {
        SSA_Block* block = &func->blocks[bi];

        if (bi > 0) {
            string_builder_append(sb, "b%u:\n", bi);
        }

#define FETCH() (block->bytes[instruction_offset++])
#define FETCH16() ({u16 r = *(u16*)&block->bytes[instruction_offset]; instruction_offset += 2; r;})
#define FETCH32() ({u32 r = *(u32*)&block->bytes[instruction_offset]; instruction_offset += 4; r;})
#define FETCH64() ({u64 r = *(u64*)&block->bytes[instruction_offset]; instruction_offset += 8; r;})

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
                    c_backend_emit_c_type(inst, sb, target_type, "");
                    string_builder_append(sb, ")r%u;", operand_reg);
                    break;
                }

                case SSA_OP_TRUNC: {
                    /*u8 size =*/ FETCH();
                    u32 result_reg = FETCH32();
                    u32 operand_reg = FETCH32();

                    string_builder_append(sb, "    r%u = (", result_reg);
                    c_backend_emit_c_type(inst, sb, func->registers[result_reg].type, "");
                    string_builder_append(sb, ")r%u;", operand_reg);
                    break;
                }

                case SSA_OP_SEXT: {
                    /*u8 dest_size =*/ FETCH();
                    /*u8 src_size =*/ FETCH();
                    u32 result_reg = FETCH32();
                    u32 operand_reg = FETCH32();

                    string_builder_append(sb, "    r%u = (", result_reg);
                    c_backend_emit_c_type(inst, sb, func->registers[result_reg].type, "");
                    string_builder_append(sb, ")r%u;", operand_reg);
                    break;
                }

                case SSA_OP_ZEXT: {
                    /*u8 dest_size =*/ FETCH();
                    u32 result_reg = FETCH32();
                    u32 operand_reg = FETCH32();

                    string_builder_append(sb, "    r%u = (", result_reg);
                    c_backend_emit_c_type(inst, sb, func->registers[result_reg].type, "");
                    string_builder_append(sb, ")r%u;", operand_reg);
                    break;
                }


                case SSA_OP_ALLOC: {
                    u32 result_reg = FETCH32();
                    /*u64 size =*/ FETCH64();

                    string_format(reg_name, "alloc_r%u", result_reg);
                    string_builder_append(sb, "    r%u = &(%s);", result_reg, reg_name);
                    break;
                }

                case SSA_OP_GLOB_PTR: assert(false); break;

                case SSA_OP_MEMCPY: {
                    u32 dest_ptr_reg = FETCH32();
                    u32 source_ptr_reg = FETCH32();
                    u64 byte_count = FETCH64();

                    if (dest_ptr_reg != sret_double_index) {
                        string_format(reg_name, "r%u", dest_ptr_reg);
                        string_builder_append(sb, "    memcpy(%s, ", reg_name);
                        string_format(reg_name, "r%u", source_ptr_reg);
                        string_builder_append(sb, "%s, %llu);", reg_name, byte_count);
                    } else {
                        no_op = true;
                    }
                    break;
                }

                case SSA_OP_STORE_PTR: {
                    /*u8 size =*/ FETCH();
                    u32 ptr_reg = FETCH32();
                    u32 value_reg = FETCH32();

                    string_builder_append(sb, "    *(r%u) = r%u;", ptr_reg, value_reg);
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

                    string_builder_append(sb, "    r%u = %llu;", result_reg, value);
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
                            sret_double_index = result_reg;

                        } else {
                            param_index--;
                        }
                    }

                    if (!sret) {
                        string_builder_append(sb, "    r%u = ", result_reg);
                        if (func->type->function.param_types[param_index]->kind == Type_Kind::STRUCT) {
                            string_builder_append(sb, "&");
                        }
                        string_builder_append(sb, "p%u;", param_index);
                    } else {
                        no_op = true;
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

                    Type* pointer_type = pointer_type_get(inst, func->registers[result_reg].type);

                    string_builder_append(sb, "    r%u = (", result_reg);
                    c_backend_emit_c_type(inst, sb, pointer_type, "");
                    string_builder_append(sb, ")&c%lld;", offset);

                    break;
                }

                case SSA_OP_STRUCT_OFFSET: {
                    u32 result_reg = FETCH32();
                    u32 base_ptr_reg = FETCH32();
                    /*u32 offset =*/ FETCH32();
                    u16 index = FETCH16();

                    string_builder_append(sb, "    r%u = &(r%u)->m%u;", result_reg, base_ptr_reg, index);
                    break;
                }

                case SSA_OP_POINTER_OFFSET: {
                    u64 size = FETCH64();
                    u32 result_reg = FETCH32();
                    u32 base_ptr_reg = FETCH32();
                    u32 index_reg = FETCH32();

                    Type* base_ptr_type = func->registers[base_ptr_reg].type;
                    assert(base_ptr_type->kind == Type_Kind::POINTER);
                    Type* base_pointee_type = base_ptr_type->pointer.base;
                    assert(base_pointee_type->bit_size / 8 == size);

                    assert(func->registers[result_reg].type == base_ptr_type);

                    string_builder_append(sb, "    r%u = r%u + r%u;", result_reg, base_ptr_reg, index_reg);

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

                    SSA_Function* callee = &inst->ssa_program->functions[fn_index];
                    String callee_name = atom_string(callee->name);

                    assert(!callee->foreign);

                    u32 arg_count = callee->param_count;
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
                        Type* arg_type = func->registers[arg_reg].type;

                        Type* param_type = nullptr;
                        if (i < callee->type->function.param_types.count) {
                            param_type = callee->type->function.param_types[i];
                        } else {
                            param_type = arg_type;
                        }
                        assert(param_type);


                        if (arg_type->kind == Type_Kind::STRUCT && param_type->kind == Type_Kind::STRUCT) {
                            string_builder_append(sb, "*(r%u)", arg_reg);
                        } else {
                            string_builder_append(sb, "r%u", arg_reg);
                        }
                    }

                    string_builder_append(sb, ");");
                    break;
                }

                case SSA_OP_CALL_FOREIGN: {
                    u32 result_reg = FETCH32();
                    u32 fn_index = FETCH32();
                    u16 arg_count = FETCH16();

                    SSA_Function* callee = &inst->ssa_program->functions[fn_index];
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

                        if (arg_type == inst->builtin_type_cstring) {
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
                        assert(value_reg == sret_double_index);
                        string_builder_append(sb, "    return alloc_r0;");
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

                    string_builder_append(sb, "    if (r%u) goto b%u;\n", cond_reg, true_block);
                    string_builder_append(sb, "    else goto b%u;", false_block);

                    break;
                }

                case SSA_OP_JMP: {
                    u32 target_block = FETCH32();

                    string_builder_append(sb, "    goto b%u;", target_block);
                    break;
                }

                case SSA_OP_ASSERT: {
                    u32 offset = instruction_offset - sizeof(SSA_Op);
                    u32 cond_reg = FETCH32();
                    u32 string_reg = FETCH32();

                    Source_Pos pos;
                    bool found = hash_table_find(&inst->ssa_program->instruction_origin_positions, { offset, fn_index, (u32)bi }, &pos);
                    assert(found);

                    Imported_File file = inst->imported_files[pos.file_index];
                    String file_name = atom_string(file.name);
                    Line_Info li = line_info(file.newline_offsets, pos.offset);

                    string_builder_append(sb, "    novo_c_assert(r%u, r%u, \"%.*s\", %u, %u);", cond_reg, string_reg,
                                          (int)file_name.length, file_name.data, li.line, li.offset);
                    break;
                }
            }

#if NOVO_C_BACKEND_PRINT_SSA_COMMENTS
            if (!no_print)  {
                if (no_op) string_builder_append(sb, "    // ");
                else string_builder_append(sb, " // ");
                ssa_print_instruction(inst, sb, inst->ssa_program, func, ip, block->bytes);
            }
#else // NOVO_C_BACKEND_PRINT_SSA_COMMENTS
            if (!no_print && !no_op) string_builder_append(sb, "\n");
#endif // NOVO_C_BACKEND_PRINT_SSA_COMMENTS

        }

        bi = block->next_index;
    }

    string_builder_append(sb, "}\n");

#undef FETCH
#undef FETCH16
#undef FETCH32
#undef FETCH64
#undef BINOP
#undef CMP_BINOP
}

void c_backend_emit_constant_expression(Instance* inst, String_Builder* sb, AST_Expression* const_expr)
{
    assert(const_expr->flags & AST_EXPR_FLAG_CONST);

    switch (const_expr->kind) {
        case AST_Expression_Kind::INVALID: assert(false); break;
        case AST_Expression_Kind::IDENTIFIER: assert(false); break;
        case AST_Expression_Kind::UNARY: assert(false); break;
        case AST_Expression_Kind::BINARY: assert(false); break;
        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::ADDRESS_OF: assert(false); break;
        case AST_Expression_Kind::DEREF: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;

        case AST_Expression_Kind::COMPOUND: {
            assert(const_expr->resolved_type->kind == Type_Kind::STRUCT);
            assert(const_expr->compound.expressions.count == const_expr->resolved_type->structure.members.count);

            string_builder_append(sb, " { ");

            for (s64 i = 0; i < const_expr->compound.expressions.count; i++) {
                if (i > 0) string_builder_append(sb, ", ");

                c_backend_emit_constant_expression(inst, sb, const_expr->compound.expressions[i]);
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

            if (const_expr->resolved_type->integer.sign) {
                string_builder_append(sb, "%lld", const_expr->integer_literal);
            } else {
                string_builder_append(sb, "%llu", const_expr->integer_literal);
            }

            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;

        case AST_Expression_Kind::STRING_LITERAL: {
            string_builder_append(sb, "{ ");

            String _str = atom_string(const_expr->string_literal);

            auto mark = temp_allocator_get_mark(&inst->temp_allocator_data);
            String str = convert_special_characters_to_escape_characters(&inst->temp_allocator, _str);
            temp_allocator_reset(&inst->temp_allocator_data, mark);

            string_builder_append(sb, "(u8*) \"%.*s\"", (int)str.length, str.data);
            string_builder_append(sb, ", %lld }", _str.length);
            break;
        }

    }
}

}
