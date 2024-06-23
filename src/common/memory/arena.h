#pragma once

#include "defines.h"
#include "memory/allocator.h"

#define NOVO_ARENA_MAX_CAP GIBIBYTE(4)

namespace Novo {

typedef u8 Arena_Flags;
enum Arena_Flag : Arena_Flags {
    ARENA_FLAG_NONE    = 0x00,
    ARENA_FLAG_NOALIGN = 0x01,
    ARENA_FLAG_GROW    = 0x02,
    ARENA_FLAG_NOZERO  = 0x04,
};

struct Arena
{
    u8* data;
    u64 used;
    u64 capacity;
    u64 max_capacity;

    Arena_Flags flags;

    void* last;
    s64 last_size;
};

struct Temp_Arena
{
    Arena* arena;
    u64 reset_to;
};

NAPI Allocator arena_allocator_create(Arena* arena);
NAPI Arena arena_create(u8* data, s64 size);

NAPI void arena_new(Arena* arena, u64 max_cap = NOVO_ARENA_MAX_CAP);
NAPI void arena_free(Arena* arena);

N__attribute((malloc, alloc_size(2), alloc_align(3)))
NAPI void* arena_alloc(Arena* arena, s64 size, s64 align);
NAPI void* arena_realloc(Arena* arena, void* old_pointer, s64 old_size, s64 size, s64 align);

NAPI bool arena_grow(Arena* arena, u64 min_cap);
NAPI void arena_reset(Arena* arena);

NAPI FN_ALLOCATOR(arena_allocator_fn);

NAPI Temp_Arena temp_arena_create(Arena* arena);
NAPI void temp_arena_release(Temp_Arena ta);
NAPI Temp_Arena temp_arena(Arena* dont_use);

}

