#pragma once

#include "defines.h"
#include "memory/allocator.h"
#include "memory/arena.h"

namespace Novo {

struct Temp_Allocator {
    Arena arena;
};

typedef u64 Temp_Allocator_Mark;

NAPI Allocator temp_allocator_create(Temp_Allocator* ta);
NAPI void temp_allocator_free(Temp_Allocator* ta);
NAPI Temp_Allocator_Mark temp_allocator_get_mark(Temp_Allocator* ta);
NAPI void temp_allocator_reset(Temp_Allocator* ta, Temp_Allocator_Mark mark = {});


NAPI FN_ALLOCATOR(temp_allocator_fn);

NAPI Allocator* temp_allocator();
NAPI Temp_Allocator_Mark temp_allocator_get_mark();
NAPI void temp_allocator_reset(Temp_Allocator_Mark mark);

}
