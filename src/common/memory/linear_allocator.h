#pragma once

#include "defines.h"
#include "allocator.h"

namespace Novo {

struct Linear_Allocator {
    FN_Allocator *backing_allocator;
    void *backing_allocator_data;

    u8 *buffer;
    s64 used;
    s64 size;
};

NAPI bool linear_allocator_create(Linear_Allocator *la, FN_Allocator backing_allocator, void *backing_user_data, s64 size);

NAPI FN_ALLOCATOR(linear_allocator);

}
