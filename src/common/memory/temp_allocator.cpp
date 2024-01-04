#include "temp_allocator.h"
#include <cassert>

namespace Novo {

Allocator temp_allocator_create(Temp_Allocator *ta, Allocator *backing_allocator, s64 size)
{
    linear_allocator_create(&ta->linear_allocator_data, backing_allocator, size);
    return { temp_allocator_fn, ta, ALLOCATOR_FLAG_CANT_FREE | ALLOCATOR_FLAG_CANT_REALLOC };
}

FN_ALLOCATOR(temp_allocator_fn)
{
    auto ta = (Temp_Allocator *)allocator_data;

    switch (mode) {

        case Allocator_Mode::ALLOCATE: {
            return linear_allocator_allocate(&ta->linear_allocator_data, size, align);
        }

        case Allocator_Mode::REALLOCATE: {
            assert(false && "Temporary allocator does not support reallocation!");
            return nullptr;
        }

        case Allocator_Mode::FREE: {
            assert(false && "Temporary allocator does not support freeing!");
            return nullptr;
        }

        case Allocator_Mode::FREE_ALL: {
            linear_allocator_free_all(&ta->linear_allocator_data);
            return nullptr;
        }
    }
    assert(false);
}

}
