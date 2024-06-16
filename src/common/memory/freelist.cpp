#include "freelist.h"

#include "defines.h"
#include "filesystem.h"
#include "memory/allocator.h"
#include "memory/arena.h"
#include "src/compiler/type.h"
#include "string_builder.h"

#include <cassert>

namespace Novo {

void freelist_init(Freelist* freelist, Arena arena)
{
    freelist->arena = arena;
    freelist->remaining = arena.capacity;

    freelist_reset(freelist);
}

void freelist_reset(Freelist* freelist)
{
    assert(freelist->arena.capacity > sizeof(Freelist_Header));
    Freelist_Header* node = (Freelist_Header*)freelist->arena.data;

    node->size = freelist->arena.capacity;
    node->next = nullptr;

    freelist->first_free = node;
    freelist->remaining = freelist->arena.capacity;
}

Freelist_Header* freelist_grow(Freelist* freelist, s64 min_increase, Freelist_Header** prev_out)
{
    if (!(freelist->arena.flags & ARENA_FLAG_GROW)) return nullptr;

    s64 old_cap = freelist->arena.capacity;

    bool grow_res = arena_grow(&freelist->arena, old_cap + min_increase);
    assert(grow_res);

    s64 increase = freelist->arena.capacity - old_cap;
    assert(increase >= min_increase);
    freelist->remaining += increase;

    Freelist_Header* last_node = freelist->first_free;
    Freelist_Header* prev_node = nullptr;
    if (last_node) {
        while (last_node->next) {
            prev_node = last_node;
            last_node = last_node->next;
        }
    }

    Freelist_Header* new_node = (Freelist_Header*)(freelist->arena.data + old_cap);
    new_node->size = increase;
    new_node->next = nullptr;

    Freelist_Header* result = nullptr;

    if (last_node) {

        if ((u8*)last_node + last_node->size == (u8*)new_node) {
            last_node->size += new_node->size;
            result = last_node;
        } else {
            last_node->next = new_node;
            result = new_node;
            prev_node = last_node;
        }

    } else {
        freelist->first_free = new_node;
        result = new_node;
    }

    assert(result);
    if (prev_out) *prev_out = prev_node;

    return result;
}

void freelist_insert(Freelist* freelist, Freelist_Header* insert_after, Freelist_Header* node)
{
    if (insert_after) {
        assert(freelist->first_free);
        assert(insert_after < node);
        node->next = insert_after->next;
        insert_after->next = node;
    } else {

        if (freelist->first_free) {
            assert(node < freelist->first_free);
            node->next = freelist->first_free;
            freelist->first_free = node;
        } else {
            freelist->first_free = node;
            node->next = nullptr;
        }
    }
}

void freelist_remove(Freelist* freelist, Freelist_Header* prev, Freelist_Header* node)
{
    if (prev) {
        prev->next = node->next;
    } else {
        assert(freelist->first_free == node);
        freelist->first_free = node->next;
    }

    node->next = nullptr;
}

Freelist_Header* freelist_find_first(Freelist* freelist, u64 size, Freelist_Header** prev_)
{
    if (!freelist->first_free) return nullptr;

   Freelist_Header* node = freelist->first_free;
   Freelist_Header* prev = nullptr;

    while (node) {
        if (node->size >= size) {
            if (prev_) *prev_ = prev;
            return node;
        }

        prev = node;
        node = node->next;
    }

    return nullptr;
}

void* freelist_allocate(Freelist* freelist, s64 size, s64 align)
{
    u64 total_size =  size + max((size_t)align - 1, sizeof(Freelist_Alloc_Header));

    // Avoid headers from overlapping
    if (total_size < sizeof(Freelist_Header)) total_size = sizeof(Freelist_Header);

    Freelist_Header *prev;
    Freelist_Header* node = freelist_find_first(freelist, total_size, &prev);

    if (!node) {
        node = freelist_grow(freelist, total_size, &prev);
        assert(node);

        if (!node) {
            assert(false && "Freelist out of memory");
        }
    }

    // This padding is for alignment and header
    u64 padding = get_aligned((u64)node + sizeof(Freelist_Alloc_Header), align) - (u64)node;
    assert(padding >= sizeof(Freelist_Alloc_Header));

    s32 alignment_padding = padding - sizeof(Freelist_Alloc_Header);

    if (node->size > total_size) {
        Freelist_Header* new_node = (Freelist_Header*)((u8*)node + total_size);
        new_node->size = node->size - total_size;
        new_node->next = nullptr;
        freelist_insert(freelist, node, new_node);
    }

    // TODO: when align passes a treshold, modify this node and the final alignment_padding, to avoid wasting the space
    freelist_remove(freelist, prev, node);

    Freelist_Alloc_Header* alloc_header = (Freelist_Alloc_Header*)((u8*)node + alignment_padding);
    alloc_header->alignment_padding = alignment_padding;
    assert(total_size <= U32_MAX);
    alloc_header->size = total_size;

    freelist->remaining -= total_size;

    return (u8*)alloc_header + sizeof(Freelist_Alloc_Header);
}

void freelist_release(Freelist* freelist, void* ptr)
{
    Freelist_Alloc_Header* alloc_header_ = (Freelist_Alloc_Header*)((u8*)ptr - sizeof(Freelist_Alloc_Header));
    Freelist_Alloc_Header old_alloc_header = *alloc_header_;

    Freelist_Header* new_node = (Freelist_Header*)((u8*)alloc_header_ - old_alloc_header.alignment_padding);
    new_node->size = old_alloc_header.size;

    freelist->remaining += old_alloc_header.size;

    Freelist_Header* node = freelist->first_free;
    Freelist_Header* prev = nullptr;

    if (!node) {
        freelist->first_free = new_node;
        return;
    }

    while (node) {
        if (new_node < node) {
            freelist_insert(freelist, prev, new_node);
            break;
        }
        prev = node;
        node = node->next;
    }

    if (new_node->next && (Freelist_Header*)((u8*)new_node + new_node->size) == new_node->next) {
        new_node->size += new_node->next->size;
        freelist_remove(freelist, new_node, new_node->next);
    }

    if (prev && (Freelist_Header*)((u8*)prev + prev->size) == new_node) {
        prev->size += new_node->size;
        freelist_remove(freelist, prev, new_node);
    }
}

FN_ALLOCATOR(fl_allocator_fn)
{
    Freelist* freelist = (Freelist*)allocator_data;

    switch (mode) {
        case Allocator_Mode::ALLOCATE: {
            return freelist_allocate(freelist, size, align);
        }

        case Allocator_Mode::REALLOCATE: assert(false); break;

        case Allocator_Mode::FREE: {
            freelist_release(freelist, old_pointer);
            return nullptr;
        }

        case Allocator_Mode::FREE_ALL: assert(false); break;
    }

    assert(false);
    return nullptr;
}

bool g_fl_allocator_initialized = false;
Freelist g_freelist;
Allocator g_fl_allocator;

Allocator* fl_allocator()
{
    if (!g_fl_allocator_initialized) {
        Arena arena;
        arena_new(&arena);
        freelist_init(&g_freelist, arena);
        g_fl_allocator = { fl_allocator_fn, &g_freelist };
        g_fl_allocator_initialized = true;
    }

    return &g_fl_allocator;
}

#ifndef NDEBUG
void freelist_dump_graph(Freelist* fl, const char* filename)
{
    Temp_Arena tarena = temp_arena(nullptr);
    Allocator ta = arena_allocator_create(tarena.arena);

    String_Builder sb;
    string_builder_init(&sb, &ta);

    string_builder_append(&sb, "digraph G {\n");
    string_builder_append(&sb, "node [shape=record];\n");
    string_builder_append(&sb, "rankdir=LR;\n");

    Freelist_Header* node = fl->first_free;
    s64 node_idx = 0;
    while (node) {
        s64 start_offset = (u8*)node - (u8*)fl->arena.data;
        string_builder_append(&sb, "  node%lld [label = \"start: %lld | end: %lld | size: %lld\" ];\n", node_idx, start_offset, start_offset + node->size, node->size);

        if (node->next) {
            string_builder_append(&sb, "  node%lld -> node%lld;\n", node_idx, node_idx + 1);
        }

        node_idx++;
        node = node->next;
    }

    string_builder_append(&sb, "}");

    String graph_string = string_builder_to_string(&sb);
    fs_write_entire_file(filename, graph_string);

    temp_arena_release(tarena);
}
#endif // NDEBUG

}

