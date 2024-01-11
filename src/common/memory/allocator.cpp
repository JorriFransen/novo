#include "allocator.h"

#include <defines.h>

#include <cassert>
#include <cstdlib>
#include <cstring>

namespace Novo {

bool g_c_allocator_initialized = false;
Allocator g_c_allocator;

Allocator *c_allocator()
{
    if (!g_c_allocator_initialized) {
        g_c_allocator = { c_allocator_fn, nullptr, ALLOCATOR_FLAG_CANT_REALLOC };
        g_c_allocator_initialized = true;
    }

    return &g_c_allocator;
}

FN_ALLOCATOR(c_allocator_fn)
{
    switch (mode) {

    case Allocator_Mode::ALLOCATE: {
        s64 actual_size = size + (align - 1) + sizeof(void *);
        u8 *mem = (u8 *)malloc(actual_size);
        void **ptr = (void **)get_aligned((u64)mem + sizeof(void *), align);

        // Store the pointer returned by malloc
        ptr[-1] = mem;

        return ptr;
    }

    case Allocator_Mode::REALLOCATE: {
        assert(false && "c_allocator does not support reallocation\n");
        break;
    }

    case Allocator_Mode::FREE: {
        auto old = (void **)old_pointer;
        auto _old = old[-1];
        ::free(_old);
        break;
    }

    case Allocator_Mode::FREE_ALL:
        assert(false && "c_allocator does not support free all\n");
        break;
    }

    return nullptr;
}

FN_DEFAULT_ALLOCATE(allocate)
{
    return allocate(c_allocator(), size);
}

FN_ALLOCATE(allocate)
{
    auto ptr = allocator->fn(Allocator_Mode::ALLOCATE, size, 1, 0, nullptr, allocator->user_data, 0);
    memset(ptr, 0, size);
    return ptr;
}

FN_DEFAULT_ALLOCATE_ALIGNED(allocate_aligned)
{
    return allocate_aligned(c_allocator(), size, align);
}

FN_ALLOCATE_ALIGNED(allocate_aligned)
{
    auto ptr = allocator->fn(Allocator_Mode::ALLOCATE, size, align, 0, nullptr, allocator->user_data, 0);
    memset(ptr, 0, size);
    return ptr;
}

FN_DEFAULT_FREE(free)
{
    free(c_allocator(), ptr);
}

FN_FREE(free)
{
    allocator->fn(Allocator_Mode::FREE, 0, 0, 0, ptr, allocator->user_data, 0);
}

FN_FREE_ALL(free_all)
{
    allocator->fn(Allocator_Mode::FREE_ALL, 0, 0, 0, nullptr, allocator->user_data, 0);
}

}
