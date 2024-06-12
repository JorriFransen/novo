#pragma once

#include "allocator.h"
#include "defines.h"
#include "memory/arena.h"

namespace Novo {

struct Allocator;

struct Freelist_Header
{
    u64 size;
    Freelist_Header* next;
};

struct Freelist
{
    Freelist_Header* first_free;

    Arena arena;
    u64 remaining; // TODO: Remove this and use 'used' from the arena
};

struct Freelist_Alloc_Header
{
    s32 alignment_padding;
    u32 size;
};


NAPI void freelist_init(Freelist* freelist, Arena arena);
NAPI void freelist_reset(Freelist* freelist);

NAPI bool freelist_grow(Freelist* freelist, s64 min_increase);

NAPI void freelist_insert(Freelist* freelist, Freelist_Header* insert_after, Freelist_Header* node);
NAPI void freelist_remove(Freelist* freelist, Freelist_Header* prev, Freelist_Header* node);

NAPI Freelist_Header* freelist_find_first(Freelist* freelist, u64 size, Freelist_Header** prev);
NAPI void* freelist_allocate(Freelist* freelist, s64 size, s64 align);
NAPI void freelist_release(Freelist* freelist, void* ptr);

NAPI FN_ALLOCATOR(fl_allocator_fn);

NAPI Allocator* fl_allocator();

#ifndef NDEBUG
NAPI void freelist_dump_graph(Freelist* fl, const char* filename);
#endif // NDEBUG


}
