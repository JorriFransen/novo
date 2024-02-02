#pragma once

#include "defines.h"
#include "memory/allocator.h"

namespace Novo {

struct Linear_Allocator {
    Allocator* backing_allocator;

    u8* buffer;
    s64 used;
    s64 size;
};

NAPI Allocator linear_allocator_create(Linear_Allocator* la, Allocator* backing_allocator, s64 size);
NAPI void linear_allocator_free(Linear_Allocator* allocator);
NAPI void* linear_allocator_allocate(Linear_Allocator* la, s64 size, u64 align);
NAPI void linear_allocator_free_all(Linear_Allocator* la);

NAPI FN_ALLOCATOR(linear_allocator_fn);

}
