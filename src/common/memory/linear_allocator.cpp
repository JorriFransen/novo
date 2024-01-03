#include "linear_allocator.h"

#include <cassert>

namespace Novo {

bool linear_allocator_create(Linear_Allocator *la, FN_Allocator backing_allocator, void *backing_user_data, s64 size)
{
    la->backing_allocator = backing_allocator;
    la->backing_allocator_data = backing_user_data;

    la->buffer = (u8 *)allocate(backing_allocator, backing_user_data, size);

    if (!la->buffer) {
        return false;
    }

    la->used = 0;
    la->size = size;

    return true;
}

FN_ALLOCATOR(linear_allocator)
{
    auto la = (Linear_Allocator *)allocator_data;

    switch (mode) {

        case Allocator_Mode::ALLOCATE: {

            s64 actual_size = size + (align - 1);

            if (actual_size > (la->size - la->used)) {
                assert(false && "Linear allocator out of space!");
                return nullptr;
            }

            auto ptr = (void *)get_aligned((u64)&la->buffer[la->used], align);
            la->used += actual_size;

            return ptr;
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
            la->used = 0;
            return nullptr;
        }
    }

    assert(false);
    return nullptr;
}

}
