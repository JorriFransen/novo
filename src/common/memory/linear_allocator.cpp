#include "linear_allocator.h"

#include <cassert>

namespace Novo {

Allocator linear_allocator_create(Linear_Allocator *la, Allocator *backing_allocator, s64 size)
{
    la->backing_allocator = backing_allocator;

    la->buffer = (u8 *)allocate(backing_allocator, size);
    assert(la->buffer);

    la->used = 0;
    la->size = size;

    return { linear_allocator_fn, la, ALLOCATOR_FLAG_CANT_FREE | ALLOCATOR_FLAG_CANT_REALLOC };
}

void *linear_allocator_allocate(Linear_Allocator *la, s64 size, u64 align)
{
    s64 actual_size = size + (align - 1);

    if (actual_size > (la->size - la->used)) {
        assert(false && "Linear allocator out of space!");
        return nullptr;
    }

    auto ptr = (void *)get_aligned((u64)&la->buffer[la->used], align);
    la->used += actual_size;

    return ptr;
}

void linear_allocator_free_all(Linear_Allocator *la)
{
    la->used = 0;
}

FN_ALLOCATOR(linear_allocator_fn)
{
    auto la = (Linear_Allocator *)allocator_data;

    switch (mode) {

        case Allocator_Mode::ALLOCATE: {
            return linear_allocator_allocate(la, size, align);
        }

        case Allocator_Mode::REALLOCATE: {
            assert(false && "Linear allocator does not support reallocation!");
            return nullptr;
        }

        case Allocator_Mode::FREE: {
            assert(false && "Linear allocator does not support free (only free all)!");
            return nullptr;
        }

        case Allocator_Mode::FREE_ALL: {
            linear_allocator_free_all(la);
            return nullptr;
        }
    }

    assert(false);
    return nullptr;
}

}
