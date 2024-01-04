#pragma once

#include "defines.h"
#include "linear_allocator.h"

namespace Novo {

struct Temp_Allocator {
    Linear_Allocator linear_allocator_data;
};

NAPI Allocator temp_allocator_create(Temp_Allocator *ta, Allocator *backing_allocator, s64 size);

NAPI FN_ALLOCATOR(temp_allocator_fn);

}
