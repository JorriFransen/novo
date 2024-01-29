#include "instance.h"

#include <filesystem.h>
#include <logger.h>

#include "ast.h"
#include "ast_print.h"
#include "atom.h"
#include "keywords.h"
#include "parser.h"
#include "resolver.h"
#include "scope.h"
#include "source_pos.h"
#include "task.h"
#include "type.h"
#include "typer.h"

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <stdarg.h>

namespace Novo {

void instance_init(Instance *inst, Options options)
{
    inst->options = options;

    if (options.verbose) set_min_log_level(LOG_LEVEL_DEBUG);
    if (options.trace) set_min_log_level(LOG_LEVEL_TRACE);

    if (!fs_is_directory(inst->cwd)) {
        assert(false && "Invalid cwd!");
    }

    fs_chdir(inst->cwd);


    auto default_alloc = inst->default_allocator;

    inst->temp_allocator = temp_allocator_create(&inst->temp_allocator_data, default_alloc, KIBIBYTE(16));
    inst->ast_allocator = linear_allocator_create(&inst->ast_allocator_data, default_alloc, KIBIBYTE(16));
    inst->scope_allocator = inst->ast_allocator;

    darray_init(default_alloc, &inst->parse_tasks);
    darray_init(default_alloc, &inst->resolve_tasks);
    darray_init(default_alloc, &inst->type_tasks);
    darray_init(default_alloc, &inst->ssa_tasks);

    inst->global_scope = scope_new(inst, Scope_Kind::GLOBAL);

    darray_init(&inst->ast_allocator, &inst->function_types);

    inst->fatal_error = false;

    darray_init(default_alloc, &inst->source_positions);
    // push dummy because index zero is invalid
    darray_append(&inst->source_positions, {});

    darray_init(default_alloc, &inst->source_ranges);
    // push dummy because index zero is invalid
    darray_append(&inst->source_ranges, {});

    if (!g_atoms_initialized) {
        initialize_atoms(default_alloc, 128);
        initialize_keywords();
        g_atoms_initialized = true;
    }

    // TODO: Custom allocator
    ssa_program_init(&inst->ssa_program, c_allocator());

    inst->builtin_type_void = void_type_new(inst);
    auto void_decl = ast_builtin_type_decl(inst, inst->builtin_type_void, "void");
    scope_add_symbol(inst->global_scope, void_decl->ident->atom, void_decl);

    inst->builtin_type_s64 = integer_type_new(inst, true, 64);
    auto int_decl = ast_builtin_type_decl(inst, inst->builtin_type_s64, "s64");
    scope_add_symbol(inst->global_scope, int_decl->ident->atom, int_decl);

    inst->builtin_type_bool = boolean_type_new(inst, 8);
    auto bool_decl = ast_builtin_type_decl(inst, inst->builtin_type_bool, "bool");
    scope_add_symbol(inst->global_scope, bool_decl->ident->atom, bool_decl);
}

bool instance_start(Instance *inst)
{
    auto first_file_name = String_Ref(inst->options.input_file);
    assert(first_file_name.length);

    String first_file_path;

    if (!fs_is_file(first_file_name)) {
        fprintf(stderr, "Invalid file path: %s\n", first_file_name.data);
        return false;
    }

    if (!fs_is_realpath(first_file_name)) {
        first_file_path = fs_realpath(inst->default_allocator, first_file_name);
    } else {
        first_file_path = string_copy(inst->default_allocator, first_file_name);
    }

    add_parse_task(inst, first_file_name);

    bool progress = true;

    while (progress) {

        progress = false;

        for (s64 i = 0; i < inst->parse_tasks.count; i++) {
            Parse_Task task = inst->parse_tasks[i];

            AST_File *parsed_file = parse_file(inst, task.file_name);
            if (parsed_file)
            {
                darray_remove_unordered(&inst->parse_tasks, i);
                i--;

            } else {
                assert(false); // Parser should have exited with a fatal error
            }

            progress = true;

            if (inst->options.print_ast) {
                String ast_str = ast_to_string(inst, parsed_file, &inst->temp_allocator);
                printf("\n%s\n", ast_str.data);
            }

            add_resolve_tasks(inst, parsed_file, inst->global_scope);
        }

        for (s64 i = 0; i < inst->resolve_tasks.count; i++) {
            Resolve_Task task = inst->resolve_tasks[i];

            bool success = resolve_node(inst, &task, &task.node, task.scope);

            if (success) {
                progress = true;

                darray_remove_unordered(&inst->resolve_tasks, i);
                i--;

                add_type_task(inst, task.node, task.scope);
            }
        }

        for (s64 i = 0; i < inst->type_tasks.count; i++) {
            Type_Task task = inst->type_tasks[i];

            bool success = type_node(inst, &task.node, task.scope);

            if (success) {
                progress = true;

                darray_remove_unordered(&inst->type_tasks, i);
                i--;

                if (task.node.kind == AST_Node_Kind::DECLARATION &&
                    task.node.declaration->kind == AST_Declaration_Kind::FUNCTION) {

                    add_ssa_task(inst, task.node);

                }
            }
        }

        for (s64 i = 0; i < inst->ssa_tasks.count; i++) {
            SSA_Task task = inst->ssa_tasks[i];

            bool success;

            auto wait_for = &task.func_decl->function.wait_for_bytecode;
            for (s64 i = 0; i < wait_for->count; i++) {
                auto wait_node = (*wait_for)[i];
                assert(wait_node.kind == AST_Node_Kind::DECLARATION);
                auto decl = wait_node.declaration;
                assert(decl->kind = AST_Declaration_Kind::FUNCTION);
                assert(decl->ident);

                if (ssa_find_function(&inst->ssa_program, decl->ident->atom, nullptr)) {
                    darray_remove_ordered(wait_for, i);
                    i--;
                }
            }

            success = wait_for->count == 0;

            if (success) {

                success = ssa_emit_function(&inst->ssa_program, task.func_decl);
                assert(success);

                progress = true;

                darray_remove_unordered(&inst->ssa_tasks, i);
                i--;
            }
        }
    }

    bool all_done = inst->parse_tasks.count == 0 &&
                    inst->resolve_tasks.count == 0 &&
                    inst->type_tasks.count == 0 &&
                    inst->ssa_tasks.count == 0;

    if (!progress && !all_done) {
        assert(false); // report errors
    }

    return true;
}

static void instance_error_va(Instance *inst, Source_Pos sp, const char *fmt, va_list args)
{
    inst->fatal_error = true;

    fprintf(stderr, "%s:%d:%d: error: ", sp.name, sp.line, sp.offset);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

static void instance_error_note_va(Instance *inst, Source_Pos sp, const char *fmt, va_list args)
{
    inst->fatal_error = true;

    fprintf(stderr, "%s:%d:%d: note: ", sp.name, sp.line, sp.offset);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

void instance_error(Instance *inst, Source_Pos sp, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    instance_error_va(inst, sp, fmt, args);

    va_end(args);
}

void instance_error(Instance *inst, u32 sp_id, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    assert(sp_id > 0 && sp_id < inst->source_positions.count);
    auto sp = inst->source_positions[sp_id];

    instance_error_va(inst, sp, fmt, args);

    va_end(args);
}

void instance_fatal_error(Instance *inst, Source_Pos sp, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    instance_error_va(inst, sp, fmt, args);

    va_end(args);

    exit(1);
}

void instance_fatal_error(Instance *inst, u32 sp_id, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    assert(sp_id > 0 && sp_id < inst->source_positions.count);
    auto sp = inst->source_positions[sp_id];

    instance_error_va(inst, sp, fmt, args);

    va_end(args);

    exit(1);
}

void instance_fatal_error_note(Instance *inst, Source_Pos sp, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    instance_error_note_va(inst, sp, fmt, args);

    va_end(args);

    exit(1);
}

void instance_fatal_error_note(Instance *inst, u32 sp_id, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    assert(sp_id > 0 && sp_id < inst->source_positions.count);
    auto sp = inst->source_positions[sp_id];

    instance_error_note_va(inst, sp, fmt, args);

    va_end(args);

    exit(1);
}

}
