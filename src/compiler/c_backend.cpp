#include "c_backend.h"

#include "instance.h"
#include "ssa.h"
#include "type.h"

#include <filesystem.h>
#include <logger.h>
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

#include <string.h>)PREAMBLE";

    string_builder_append(&sb, "/* Preamble */\n");
    string_builder_append(&sb, "%s\n", preamble);
    string_builder_append(&sb, "/* End preamble */\n\n");

    // Type declarations
    for (s64 i = 0; i < inst->struct_types.count; i++) {
        c_backend_emit_struct_declaration(inst, &sb, inst->struct_types[i]);
        string_builder_append(&sb, "\n");
    }

    // Function declarations
    string_builder_append(&sb, "/* Function declarations */\n");
    for (s64 i = 0; i < program->functions.count; i++) {
        SSA_Function *func = &program->functions[i];
        c_backend_emit_c_type(inst, &sb, func->type, atom_string(func->name));

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
        c_backend_emit_function(inst, &sb, func, &arg_stack);
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

    Command_Result c_res = platform_run_command(Array_Ref<String_Ref>({"clang", "-std=c99", "-g", "c_backend_output.c"}));

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
    if (string_equal(name, "main")) {
        assert(type->kind == Type_Kind::FUNCTION);

        name = "novo_main";
    }

    switch (type->kind) {
        case Type_Kind::INVALID: assert(false); break;
        case Type_Kind::VOID: assert(false); break;

        case Type_Kind::INTEGER: {
            string_builder_append(sb, "%c%d", type->integer.sign ? 's' : 'u', type->bit_size);

            if (name.length) {
                string_builder_append(sb, " %.*s", (int)name.length, name.data);
            }
            break;
        }

        case Type_Kind::BOOLEAN: assert(false); break;

        case Type_Kind::POINTER: {
            c_backend_emit_c_type(inst, sb, type->pointer.base, "");
            string_builder_append(sb, "(* %.*s)", (int)name.length, name.data);
            break;
        }

        case Type_Kind::FUNCTION: {
            c_backend_emit_c_type(inst, sb, type->function.return_type, "");
            string_builder_append(sb, " (%.*s)(", (int)name.length, name.data);

            for (s64 i = 0; i < type->function.param_types.count; i++) {
                Type* param_type = type->function.param_types[i];

                if (i > 0) string_builder_append(sb, ", ");

                char param_name[32];
                string_format(param_name, "a%u", i);

                c_backend_emit_c_type(inst, sb, param_type, param_name);
            }

            if (type->flags & TYPE_FLAG_FOREIGN_VARARG) {
                string_builder_append(sb, ", ...");
            }

            string_builder_append(sb, ")");
            break;
        }

        case Type_Kind::STRUCT: {
            String struct_name = atom_string(type->structure.name);
            string_builder_append(sb, "%.*s", (int)struct_name.length, struct_name.data);

            if (name.length) string_builder_append(sb, " %.*s", (int)name.length, name.data);

            break;
        }
    }
}

void c_backend_emit_function(Instance* inst, String_Builder* sb, SSA_Function *func, Stack<u32> *arg_stack)
{
    c_backend_emit_c_type(inst, sb, func->type, atom_string(func->name));

    if (func->foreign) {
        string_builder_append(sb, ";\n");
        return;
    }

    string_builder_append(sb, "\n{\n");

    char reg_name[32];

    for (s64 bi = 0; bi < func->blocks.count; bi++) {
        SSA_Block* block = &func->blocks[bi];

        if (bi > 0) {
            String block_name = atom_string(block->name);
            string_builder_append(sb, "%.*s:\n", (int)block_name.length, block_name.data);
        }

#define FETCH() (block->bytes[instruction_offset++])
#define FETCH16() ({u16 r = *(u16*)&block->bytes[instruction_offset]; instruction_offset += 2; r;})
#define FETCH32() ({u32 r = *(u32*)&block->bytes[instruction_offset]; instruction_offset += 4; r;})
#define FETCH64() ({u64 r = *(u64*)&block->bytes[instruction_offset]; instruction_offset += 8; r;})

        s64 instruction_offset = 0;
        while (instruction_offset < block->bytes.count) {

            SSA_Op op = (SSA_Op)FETCH();

            switch (op) {
                case SSA_OP_NOP: assert(false); break;

                case SSA_OP_ADD: {
                    /*u8 size =*/ FETCH();
                    u32 result_reg = FETCH32();
                    u32 left_reg = FETCH32();
                    u32 right_reg = FETCH32();

                    string_builder_append(sb, "    ");
                    string_format(reg_name, "r%u", result_reg);
                    c_backend_emit_c_type(inst, sb, func->register_types[result_reg], reg_name);
                    string_builder_append(sb, " = r%u + r%u;\n", left_reg, right_reg);
                    break;
                }

                case SSA_OP_SUB: assert(false); break;
                case SSA_OP_MUL: assert(false); break;
                case SSA_OP_DIV: assert(false); break;
                case SSA_OP_LT: assert(false); break;
                case SSA_OP_GT: assert(false); break;
                case SSA_OP_EQ: assert(false); break;
                case SSA_OP_NEQ: assert(false); break;
                case SSA_OP_LTEQ: assert(false); break;
                case SSA_OP_GTEQ: assert(false); break;
                case SSA_OP_BITCAST: assert(false); break;
                case SSA_OP_TRUNC: assert(false); break;
                case SSA_OP_SEXT: assert(false); break;
                case SSA_OP_ZEXT: assert(false); break;

                case SSA_OP_ALLOC: {
                    u32 result_reg = FETCH32();
                    /*u64 size =*/ FETCH64();

                    Type* alloc_type = func->register_types[result_reg];
                    Type* pointer_type = pointer_type_get(inst, alloc_type);

                    char alloc_name[32];

                    string_builder_append(sb, "    ");
                    string_format(alloc_name, "alloc_r%u", result_reg);
                    c_backend_emit_c_type(inst, sb, alloc_type, alloc_name);
                    string_builder_append(sb, ";\n");

                    string_builder_append(sb, "    ");
                    string_format(reg_name, "r%u", result_reg);
                    c_backend_emit_c_type(inst, sb, pointer_type, reg_name);
                    string_builder_append(sb, " = &(%s);\n", alloc_name);
                    break;
                }

                case SSA_OP_GLOB_PTR: assert(false); break;

                case SSA_OP_MEMCPY: {
                    u32 dest_ptr_reg = FETCH32();
                    u32 source_ptr_reg = FETCH32();
                    u64 byte_count = FETCH64();

                    string_format(reg_name, "r%u", dest_ptr_reg);
                    string_builder_append(sb, "    memcpy(%s, ", reg_name);
                    string_format(reg_name, "r%u", source_ptr_reg);
                    string_builder_append(sb, "%s, %llu);\n", reg_name, byte_count);
                    break;
                }

                case SSA_OP_STORE_PTR: {
                    /*u8 size =*/ FETCH();
                    u32 ptr_reg = FETCH32();
                    u32 value_reg = FETCH32();

                    string_builder_append(sb, "    *(r%u) = r%u;\n", ptr_reg, value_reg);
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

                    string_builder_append(sb, "    ");
                    string_format(reg_name, "r%u", result_reg);
                    c_backend_emit_c_type(inst, sb, func->register_types[result_reg], reg_name);
                    string_builder_append(sb, " = %llu;\n", value);
                    break;
                }

                case SSA_OP_LOAD_PARAM: {
                    u32 result_reg = FETCH32();
                    u32 param_index = FETCH32();

                    string_builder_append(sb, "    ");
                    string_format(reg_name, "r%u", result_reg);
                    Type *type = nullptr;

                    bool sret = false;
                    if (func->sret) {

                        if (param_index == 0) {

                            // Loading return value
                            type = pointer_type_get(inst, func->type->function.return_type);
                            sret = true;

                        } else {
                            param_index--;
                        }
                    }

                    if (!sret) {
                        type = func->type->function.param_types[param_index];
                    }

                    c_backend_emit_c_type(inst, sb, type, reg_name);
                    string_builder_append(sb, " = ");

                    if (sret) {
                        string_builder_append(sb, "r0;\n");
                    } else {
                        string_builder_append(sb, "a%u;\n", param_index);
                    }
                    break;
                }

                case SSA_OP_LOAD_PTR: {
                    /*u8 size =*/ FETCH();
                    u32 result_reg = FETCH32();
                    u32 ptr_reg = FETCH32();

                    string_builder_append(sb, "    ");
                    string_format(reg_name, "r%u", result_reg);
                    c_backend_emit_c_type(inst, sb, func->register_types[result_reg], reg_name);
                    string_format(reg_name, "r%u", ptr_reg);
                    string_builder_append(sb, " = *(%s);\n", reg_name);
                    break;
                }

                case SSA_OP_LOAD_CONST: assert(false); break;

                case SSA_OP_STRUCT_OFFSET: {
                    u32 result_reg = FETCH32();
                    u32 base_ptr_reg = FETCH32();
                    /*u32 offset =*/ FETCH32();
                    u16 index = FETCH16();

                    string_builder_append(sb, "    ");
                    string_format(reg_name, "r%u", result_reg);
                    c_backend_emit_c_type(inst, sb, func->register_types[result_reg], reg_name);

                    string_format(reg_name, "r%u", base_ptr_reg);
                    string_builder_append(sb, " = &(%s)->", reg_name);
                    string_format(reg_name, "m%u", index);
                    string_builder_append(sb, "%s;\n", reg_name);
                    break;
                }

                case SSA_OP_POINTER_OFFSET: assert(false); break;
                case SSA_OP_POINTER_DIFF: assert(false); break;

                case SSA_OP_PUSH: {
                    u32 arg_reg = FETCH32();
                    stack_push(arg_stack, arg_reg);
                    break;
                }

                case SSA_OP_POP_N: {
                    u32 count = FETCH32();
                    stack_pop(arg_stack, count);
                    break;
                }

                case SSA_OP_CALL: {
                    u32 result_reg = FETCH32();
                    u32 fn_index = FETCH32();

                    SSA_Function* callee = &inst->ssa_program->functions[fn_index];
                    String callee_name = atom_string(callee->name);
                    string_builder_append(sb, "    ");

                    assert(!callee->foreign);

                    u32 arg_count = callee->param_count;
                    assert(stack_count(arg_stack) >= arg_count);

                    if (!callee->sret) {
                        string_format(reg_name, "r%u", result_reg);
                        c_backend_emit_c_type(inst, sb, callee->type->function.return_type, reg_name);
                    } else {
                        u32 res_reg = stack_peek(arg_stack, arg_count - 1);
                        string_builder_append(sb, "*(r%u)", res_reg);
                    }
                    string_builder_append(sb, " = %.*s(", (int)callee_name.length, callee_name.data);

                    if (callee->sret) {
                        arg_count -= 1;
                    }

                    s64 arg_offset = arg_count - 1;
                    for (s64 i = 0; i < arg_count; i++) {

                        if (i > 0) string_builder_append(sb, ", ");

                        u32 arg_reg = stack_peek(arg_stack, arg_offset--);
                        string_builder_append(sb, "r%u", arg_reg);
                    }

                    string_builder_append(sb, ");\n");
                    break;
                }

                case SSA_OP_CALL_FOREIGN: assert(false); break;

                case SSA_OP_RET: {
                    u32 value_reg = FETCH32();
                    if (func->sret) {
                        string_builder_append(sb, "    return *(r%u);\n", value_reg);
                    } else {
                        string_builder_append(sb, "    return r%u;\n", value_reg);
                    }
                    break;
                }

                case SSA_OP_RET_VOID: assert(false); break;
                case SSA_OP_JMP_IF: assert(false); break;
                case SSA_OP_JMP: assert(false); break;
                case SSA_OP_ASSERT: assert(false); break;
            }
        }
    }

    string_builder_append(sb, "}\n");

#undef FETCH
#undef FETCH16
#undef FETCH32
#undef FETCH64
}

}
