#pragma once

#include "defines.h"
#include "linear_allocator.h"
#include "memory/allocator.h"

namespace Novo {

struct Temp_Allocator {
    Linear_Allocator linear_allocator_data;
};

typedef u64 Temp_Allocator_Mark;

NAPI Allocator temp_allocator_create(Temp_Allocator *ta, Allocator *backing_allocator, s64 size);
NAPI Temp_Allocator_Mark temp_allocator_get_mark(Temp_Allocator *ta);
NAPI void temp_allocator_reset(Temp_Allocator *ta, Temp_Allocator_Mark mark = {});


NAPI FN_ALLOCATOR(temp_allocator_fn);

}
