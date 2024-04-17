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

#include <stdio.h>
#include <string.h>
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

        if (const_info.type == nullptr) {
            assert(const_info.from_expression == nullptr);
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

        if (program->functions[i].foreign) continue;

        c_backend_emit_function_decl(inst, &sb, &program->functions[i]);
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

        if (func->foreign) continue;

        c_backend_emit_function_decl(inst, &sb, func);
        string_builder_append(&sb, "\n");
        c_backend_emit_function_body(inst, &sb, func, &arg_stack);
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
        case Type_Kind::VOID: assert(false); break;

        case Type_Kind::INTEGER: {
            return string_format(ta, "%c%lld %.*s", type->integer.sign ? 's' : 'u', type->bit_size, (int)name.length, name.data);
        }

        case Type_Kind::BOOLEAN: assert(false); break;

        case Type_Kind::POINTER: {
            return c_backend_emit_c_type(inst, type->pointer.base, string_format(ta, "(*%.*s)", (int)name.length, name.data));
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
        for (s64 i = 0; i < func->param_count; i++) {
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

void c_backend_emit_function_body(Instance* inst, String_Builder* sb, SSA_Function *func, Stack<u32> *arg_stack)
{
    string_builder_append(sb, "{\n");

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

                case SSA_OP_LOAD_CONST: {
                    u32 result_reg = FETCH32();
                    u32 offset = FETCH32();

                    Type* pointer_type = pointer_type_get(inst, func->register_types[result_reg]);

                    string_builder_append(sb, "    const ");
                    string_format(reg_name, "r%u", result_reg);
                    c_backend_emit_c_type(inst, sb, pointer_type, reg_name);

                    string_format(reg_name, "&c%lld", offset);

                    string_builder_append(sb, " = %s;\n", reg_name);

                    // assert(false);
                    break;
                }

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

                case SSA_OP_CALL_FOREIGN: {
                    u32 result_reg = FETCH32();
                    u32 fn_index = FETCH32();
                    u16 arg_count = FETCH16();

                    SSA_Function* callee = &inst->ssa_program->functions[fn_index];
                    assert(callee->foreign);

                    String callee_name = atom_string(callee->name);
                    string_builder_append(sb, "    ");

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
