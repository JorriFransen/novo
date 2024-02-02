#include "temp_allocator.h"

#include <cassert>
#include <cstring>

namespace Novo {

Allocator temp_allocator_create(Temp_Allocator* ta, Allocator* backing_allocator, s64 size)
{
    linear_allocator_create(&ta->linear_allocator_data, backing_allocator, size);
    return { temp_allocator_fn, ta, ALLOCATOR_FLAG_CANT_FREE | ALLOCATOR_FLAG_CANT_REALLOC };
}

void temp_allocator_free(Temp_Allocator* ta)
{
    linear_allocator_free(&ta->linear_allocator_data);
}

Temp_Allocator_Mark temp_allocator_get_mark(Temp_Allocator* ta)
{
    return ta->linear_allocator_data.used;
}

void temp_allocator_reset(Temp_Allocator* ta, Temp_Allocator_Mark mark/*={}*/)
{
    ta->linear_allocator_data.used = mark;
}

FN_ALLOCATOR(temp_allocator_fn)
{
    auto ta = (Temp_Allocator*)allocator_data;

    switch (mode) {

        case Allocator_Mode::ALLOCATE: {
            auto result = linear_allocator_allocate(&ta->linear_allocator_data, size, align);
            memset(result, 0, size);
            return result;
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
    return nullptr;
}

}
