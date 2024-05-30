#pragma once

#include "defines.h"
#include "memory/allocator.h"

#define NOVO_ARENA_MMAP_SIZE GIBIBYTE(64)

namespace Novo {

typedef u8 Arena_Flags;
enum Arena_Flag : Arena_Flags {
    ARENA_FLAG_NONE    = 0x00,
    ARENA_FLAG_NOALIGN = 0x01,
    ARENA_FLAG_GROW    = 0x02,
};

struct Arena
{
    u8* data;
    s64 used;
    s64 capacity;
    Arena_Flags flags;
};

NAPI Allocator arena_allocator_create(Arena* arena);

NAPI void arena_create(Arena* arena);
NAPI void arena_free(Arena* arena);

__attribute((malloc, alloc_size(2), alloc_align(3)))
NAPI void* arena_alloc(Arena* arena, s64 size, s64 align);

NAPI bool arena_grow(Arena* arena, s64 min_size);
NAPI void arena_reset(Arena* arena);

NAPI FN_ALLOCATOR(arena_allocator_fn);

}

