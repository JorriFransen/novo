#include "instance.h"

#include <filesystem.h>
#include <logger.h>

#include "ast.h"
#include "atom.h"
#include "keywords.h"
#include "scope.h"
#include "source_pos.h"
#include "task.h"
#include "type.h"

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

    darray_create(default_alloc, &inst->tasks);

    inst->global_scope = scope_new(inst, Scope_Kind::GLOBAL);

    inst->fatal_error = false;

    darray_create(default_alloc, &inst->source_positions);
    // push dummy because index zero is invalid
    darray_append(&inst->source_positions, {});

    darray_create(default_alloc, &inst->source_ranges);
    // push dummy because index zero is invalid
    darray_append(&inst->source_ranges, {});

    if (!g_atoms_initialized) {
        initialize_atoms(default_alloc, 128);
        initialize_keywords();
        g_atoms_initialized = true;
    }

    auto int_decl = ast_builtin_type_decl(inst, type_integer(inst, true, 64), "int");
    scope_add_symbol(inst->global_scope, int_decl->ident->atom, int_decl);
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

    Task parse_task;
    parse_task_create(inst, &parse_task, first_file_path);
    darray_append(&inst->tasks, parse_task);

    bool progress = true;

    while (inst->tasks.count && progress && !inst->fatal_error) {

        progress = false;

        auto max = inst->tasks.count;

        for (s64 i = 0; i < max; i++) {
            bool success = task_execute(inst, &inst->tasks[i]);

            if (success) {
                progress = true;
            }
        }

        if (progress) {

            // Remove finished tasks
            s64 dest_idx = 0;
            auto count = inst->tasks.count;
            auto new_count = count;
            for (s64 i = 0; i < count; i++) {
                if (inst->tasks[i].done) {
                    new_count--;
                } else {
                    inst->tasks[dest_idx++] = inst->tasks[i];
                }
            }
            inst->tasks.count = new_count;

        } else {
            assert(inst->tasks.count);

            for (s64 i = 0; i < inst->tasks.count; i++) {
                auto &t = inst->tasks[i];
                if (t.kind == Task_Kind::RESOLVE && t.resolve.waiting_for) {
                    auto name = atom_string(t.resolve.waiting_for->atom);
                    auto sp = source_range_start(inst, t.resolve.waiting_for->range_id);
                    instance_error(inst, sp, "Reference to undeclared identifier: '%s'", name.data);
                }
            }
            return false;
        }
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
}

void instance_fatal_error_note(Instance *inst, u32 sp_id, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    assert(sp_id > 0 && sp_id < inst->source_positions.count);
    auto sp = inst->source_positions[sp_id];

    instance_error_note_va(inst, sp, fmt, args);

    va_end(args);
}

}
