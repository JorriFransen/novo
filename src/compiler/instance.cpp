#include "instance.h"

#include <containers/darray.h>
#include <defines.h>
#include <memory/allocator.h>
#include <memory/linear_allocator.h>
#include <memory/temp_allocator.h>
#include <nstring.h>
#include <platform.h>

#include "atom.h"
#include "keywords.h"
#include "scope.h"
#include "source_pos.h"
#include "task.h"

#include <cassert>
#include <cstdio>
#include <stdarg.h>

namespace Novo {

void instance_init(Instance *inst)
{
    auto default_alloc = inst->default_allocator;

    inst->temp_allocator = temp_allocator_create(&inst->temp_allocator_data, default_alloc, KIBIBYTE(16));
    inst->ast_allocator = linear_allocator_create(&inst->ast_allocator_data, default_alloc, KIBIBYTE(16));
    inst->scope_allocator = inst->ast_allocator;

    inst->cycle_error_msg_allocator = temp_allocator_create(&inst->cycle_error_msg_allocator_data, default_alloc, KIBIBYTE(2));
    string_builder_init(&inst->cycle_error_sb, inst->default_allocator);
    darray_create(default_alloc, &inst->cycle_errors);

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
}

bool instance_start(Instance *inst, const String_Ref first_file_name)
{
    if (!fs_is_directory(inst->cwd)) {
        assert(false && "Invalid cwd!");
    }

    fs_chdir(inst->cwd);

    assert(first_file_name.length);
    inst->first_file_name = first_file_name;

    String first_file_path;

    if (!fs_is_file(inst->first_file_name)) {
        fprintf(stderr, "Invalid file path: %s\n", inst->first_file_name.data);
        return false;
    }

    if (!fs_is_realpath(inst->first_file_name)) {
        first_file_path = fs_realpath(inst->default_allocator, inst->first_file_name);
    } else {
        first_file_path = string_copy(inst->default_allocator, inst->first_file_name);
    }

    Task parse_task;
    parse_task_create(&parse_task, first_file_path);
    darray_append(&inst->tasks, parse_task);

    bool progress = true;

    while (inst->tasks.count && progress && !inst->fatal_error) {

        progress = false;

        auto max = inst->tasks.count;

        for (s64 i = 0; i < max; i++) {
            bool success = task_execute(inst, &inst->tasks[i]);

            if (success) {
                progress = true;
                darray_remove_ordered(&inst->tasks, i);
                i--;
                max--;
            }
        }

        if (progress) {
            temp_allocator_reset(&inst->cycle_error_msg_allocator_data);
            inst->cycle_errors.count = 0;
        }
    }

    if (inst->tasks.count) {
        for (s64 i = 0; i < inst->cycle_errors.count; i++) {
            fprintf(stderr, "%s\n", inst->cycle_errors[i].data);
        }

        return false;
    }

    return true;
}

static void instance_error_va(Instance *inst, Source_Pos sp, const char *fmt, va_list args)
{
    string_builder_reset(&inst->cycle_error_sb);

    string_builder_append(&inst->cycle_error_sb, "%s:%d:%d: error: ", sp.name, sp.line, sp.offset);
    string_builder_append_va(&inst->cycle_error_sb, fmt, args);

    String msg = string_builder_to_string(&inst->cycle_error_sb, &inst->cycle_error_msg_allocator);
    darray_append(&inst->cycle_errors, msg);
}

static void instance_fatal_error_va(Instance *inst, Source_Pos sp, const char *fmt, va_list args)
{
    inst->fatal_error = true;

    fprintf(stderr, "%s:%d:%d: error: ", sp.name, sp.line, sp.offset);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

static void instance_fatal_error_note_va(Instance *inst, Source_Pos sp, const char *fmt, va_list args)
{
    inst->fatal_error = true;

    fprintf(stderr, "%s:%d:%d: note: ", sp.name, sp.line, sp.offset);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");
}

void instance_error(Instance *inst, Source_Pos sp, const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    instance_error_va(inst, sp, fmt, args);

    va_end(args);
}

void instance_error(Instance *inst, u32 sp_id, const char *fmt, ...)
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

    instance_fatal_error_va(inst, sp, fmt, args);

    va_end(args);
}

void instance_fatal_error(Instance *inst, u32 sp_id, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    assert(sp_id > 0 && sp_id < inst->source_positions.count);
    auto sp = inst->source_positions[sp_id];

    instance_fatal_error_va(inst, sp, fmt, args);

    va_end(args);
}

void instance_fatal_error_note(Instance *inst, Source_Pos sp, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    instance_fatal_error_note_va(inst, sp, fmt, args);

    va_end(args);
}

void instance_fatal_error_note(Instance *inst, u32 sp_id, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    assert(sp_id > 0 && sp_id < inst->source_positions.count);
    auto sp = inst->source_positions[sp_id];

    instance_fatal_error_note_va(inst, sp, fmt, args);

    va_end(args);
}

}
