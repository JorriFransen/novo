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
#include "source_pos.h"
#include "task.h"

#include <cassert>
#include <cstdio>
#include <stdarg.h>

namespace Novo {

void instance_init(Instance *instance)
{
    instance->temp_allocator = temp_allocator_create(&instance->temp_allocator_data, instance->default_allocator, KIBIBYTE(16));
    instance->ast_allocator = linear_allocator_create(&instance->ast_allocator_data, instance->default_allocator, KIBIBYTE(16));
    instance->scope_allocator = instance->ast_allocator;

    darray_create(instance->default_allocator, &instance->tasks);

    instance->fatal_error = false;

    darray_create(instance->default_allocator, &instance->source_positions);
    // push dummy because index zero is invalid
    darray_append(&instance->source_positions, {});

    darray_create(instance->default_allocator, &instance->source_ranges);
    // push dummy because index zero is invalid
    darray_append(&instance->source_ranges, {});

    if (!g_atoms_initialized) {
        initialize_atoms(instance->default_allocator, 128);
        initialize_keywords();
        g_atoms_initialized = true;
    }
}

bool instance_start(Instance *instance, const String_Ref first_file_name)
{
    if (!fs_is_directory(instance->cwd)) {
        assert(false && "Invalid cwd!");
    }

    fs_chdir(instance->cwd);

    assert(first_file_name.length);
    instance->first_file_name = first_file_name;

    String first_file_path;

    if (!fs_is_file(instance->first_file_name)) {
        fprintf(stderr, "Invalid file path: %s\n", instance->first_file_name.data);
        return false;
    }

    if (!fs_is_realpath(instance->first_file_name)) {
        first_file_path = fs_realpath(instance->default_allocator, instance->first_file_name);
    } else {
        first_file_path = string_copy(instance->default_allocator, instance->first_file_name);
    }


    Task parse_task;
    parse_task_create(&parse_task, first_file_path);
    darray_append(&instance->tasks, parse_task);

    bool progress = true;

    while (instance->tasks.count && progress && !instance->fatal_error) {

        progress = false;

        auto max = instance->tasks.count;

        for (s64 i = 0; i < max; i++) {
            bool success = task_execute(instance, &instance->tasks[i]);

            if (success) {
                progress = true;
                darray_remove_unordered(&instance->tasks, i);
                i--;
                max--;
            }
        }
    }

    if (instance->tasks.count) {
        fprintf(stderr, "Failed to execute all task successfully...\n");
    }

    return true;
}

static void instance_fatal_error_va(Instance *instance, Source_Pos sp, const char *fmt, va_list args)
{
    instance->fatal_error = true;

    fprintf(stderr, "%s:%d:%d: error: ", sp.name, sp.line, sp.offset);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");

}

static void instance_fatal_error_note_va(Instance *instance, Source_Pos sp, const char *fmt, va_list args)
{
    instance->fatal_error = true;

    fprintf(stderr, "%s:%d:%d: note: ", sp.name, sp.line, sp.offset);
    vfprintf(stderr, fmt, args);
    fprintf(stderr, "\n");

}

void instance_fatal_error(Instance *instance, Source_Pos sp, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    instance_fatal_error_va(instance, sp, fmt, args);

    va_end(args);
}

void instance_fatal_error(Instance *instance, u32 sp_id, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    assert(sp_id > 0 && sp_id < instance->source_positions.count);
    auto sp = instance->source_positions[sp_id];

    instance_fatal_error_va(instance, sp, fmt, args);

    va_end(args);
}

void instance_fatal_error_note(Instance *instance, Source_Pos sp, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    instance_fatal_error_note_va(instance, sp, fmt, args);

    va_end(args);
}

void instance_fatal_error_note(Instance *instance, u32 sp_id, const char* fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    assert(sp_id > 0 && sp_id < instance->source_positions.count);
    auto sp = instance->source_positions[sp_id];

    instance_fatal_error_note_va(instance, sp, fmt, args);

    va_end(args);
}

}
