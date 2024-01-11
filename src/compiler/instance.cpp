#include "instance.h"

#include "task.h"
#include "keywords.h"

#include <cassert>
#include <cstdio>
#include <platform.h>

namespace Novo {

void instance_init(Instance *instance)
{
    instance->temp_allocator = temp_allocator_create(&instance->temp_allocator_data, instance->default_allocator, KIBIBYTE(16));
    instance->ast_allocator = linear_allocator_create(&instance->ast_allocator_data, instance->default_allocator, KIBIBYTE(16));

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

    task_execute(instance, &parse_task);

    return true;
}

}
