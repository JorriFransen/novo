#pragma once

#include "defines.h"
#include "allocator.h"

namespace Novo {

struct Linear_Allocator {
    Allocator backing_allocator;

    u8 *buffer;
    s64 used;
    s64 size;
};

NAPI Allocator linear_allocator_create(Linear_Allocator *la, Allocator backing_allocator, s64 size);

NAPI FN_ALLOCATOR(linear_allocator);

}
