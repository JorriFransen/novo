#pragma once

#include "allocator.h"
#include "defines.h"
#include "memory/arena.h"

#ifdef NOVO_TRACE_ALLOC
#include "memory/trace.h"
#endif // NOVO_TRACE_ALLOC

namespace Novo {

struct Allocator;

struct Freelist_Node
{
    u64 size;
    Freelist_Node* next;
};

struct Freelist
{
    Freelist_Node* first_free;
    Arena arena;

#ifdef NOVO_TRACE_ALLOC
    Allocator_Trace trace;
#endif // NOVO_TRACE_ALLOC

};

struct Freelist_Alloc_Header
{
    u64 size;
    u64 padding;
};

NAPI void freelist_init(Freelist* freelist, Arena arena);
NAPI void freelist_reset(Freelist* freelist);

NAPI Freelist_Node* freelist_grow(Freelist* freelist, s64 min_increase, s64 align, s64* padding_out, Freelist_Node** prev_out);

NAPI void freelist_insert(Freelist* freelist, Freelist_Node* insert_after, Freelist_Node* node);
NAPI void freelist_remove(Freelist* freelist, Freelist_Node* prev, Freelist_Node* node);

NAPI Freelist_Node* freelist_find_first(Freelist* freelist, s64 size, s64 align, s64* padding_out, Freelist_Node** prev_node_out);
NAPI void* freelist_allocate(Freelist* freelist, s64 size, s64 align);
NAPI void* freelist_reallocate(Freelist* freelist, void* old_pointer, s64 old_size, s64 size, s64 align);
NAPI void freelist_release(Freelist* freelist, void* ptr);

NAPI FN_ALLOCATOR(fl_allocator_fn);

NAPI Allocator* fl_allocator();

#ifndef NDEBUG
NAPI void freelist_dump_graph(Freelist* fl, const char* filename);
#endif // NDEBUG


}
