#pragma once

#include "allocator.h"
#include "defines.h"

namespace Novo {

struct Allocator;

struct Freelist_Header
{
    s64 size;
    Freelist_Header* next;
};

struct Freelist
{
    Freelist_Header* first_free;
    void* start;
    s64 size;
};

NAPI void freelist_init(Freelist* freelist, void* memory, s64 size);

NAPI void freelist_insert(Freelist* freelist, Freelist_Header* insert_after, Freelist_Header* node);
NAPI void freelist_remove(Freelist* freelist, Freelist_Header* prev, Freelist_Header* node);

NAPI Freelist_Header* freelist_find_first(Freelist* freelist, s64 size, Freelist_Header** prev);
NAPI void* freelist_allocate(Freelist* freelist, s64 size, s64 align);
NAPI void freelist_release(Freelist* freelist, void* ptr);

NAPI FN_ALLOCATOR(fl_allocator_fn);

NAPI Allocator* fl_allocator();

NAPI void dump_graph(Freelist* fl, const char* filename);

}
