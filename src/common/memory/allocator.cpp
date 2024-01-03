#include "allocator.h"

#include <cassert>
#include <cstdlib>

namespace Novo {


FN_ALLOCATOR(c_allocator)
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
    return allocate(c_allocator, nullptr, size);
}

FN_ALLOCATE(allocate)
{
    return allocator(Allocator_Mode::ALLOCATE, size, 1, 0, nullptr, user_data, 0);
}

FN_DEFAULT_ALLOCATE_ALIGNED(allocate_aligned)
{
    return allocate_aligned(c_allocator, nullptr, size, align);
}

FN_ALLOCATE_ALIGNED(allocate_aligned)
{
    return allocator(Allocator_Mode::ALLOCATE, size, align, 0, nullptr, user_data, 0);
}

FN_DEFAULT_FREE(free)
{
    free(c_allocator, nullptr, ptr);
}

FN_FREE(free)
{
    allocator(Allocator_Mode::FREE, 0, 0, 0, ptr, user_data, 0);
}

FN_FREE_ALL(free_all)
{
    allocator(Allocator_Mode::FREE_ALL, 0, 0, 0, nullptr, user_data, 0);
}

}
