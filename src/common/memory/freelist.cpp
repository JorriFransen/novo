#include "freelist.h"

#include "defines.h"
#include "filesystem.h"
#include "memory/allocator.h"
#include "memory/arena.h"
#include "string_builder.h"

#include <cassert>

namespace Novo {

void freelist_init(Freelist* freelist, void* memory, s64 size)
{
    freelist->start = memory;
    freelist->size = size;
    freelist->remaining = size;

    freelist_reset(freelist);
}

void freelist_reset(Freelist* freelist)
{
    assert(freelist->size > sizeof(Freelist_Header));
    Freelist_Header* node = (Freelist_Header*)freelist->start;

    node->size = freelist->size;
    node->next = nullptr;

    freelist->first_free = node;
    freelist->remaining = freelist->size;
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

Freelist_Header* freelist_find_first(Freelist* freelist, s64 size, Freelist_Header** prev_)
{
    assert(freelist->first_free);

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
    s64 total_size =  size + max((size_t)align - 1, sizeof(Freelist_Alloc_Header));

    // Avoid headers from overlapping
    if (total_size < sizeof(Freelist_Header)) total_size = sizeof(Freelist_Header);

    Freelist_Header *prev;
    Freelist_Header* node = freelist_find_first(freelist, total_size, &prev);

    if (!node) {
        assert(false && "Freelist out of memory");
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

    Freelist_Header* new_node = (Freelist_Header*)((u8*)alloc_header_ - alloc_header_->alignment_padding);
    new_node->size = alloc_header_->size;

    Freelist_Header* node = freelist->first_free;
    Freelist_Header* prev = nullptr;
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

    freelist->remaining += old_alloc_header.size;
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

#define g_fl_allocator_size MEBIBYTE(8)

bool g_fl_allocator_initialized = false;
u8 g_fl_memory[g_fl_allocator_size];
Freelist g_freelist;
Allocator g_fl_allocator;

Allocator* fl_allocator()
{
    if (!g_fl_allocator_initialized) {
        freelist_init(&g_freelist, g_fl_memory, g_fl_allocator_size);
        g_fl_allocator = { fl_allocator_fn, &g_freelist, ALLOCATOR_FLAG_CANT_REALLOC };
        g_fl_allocator_initialized = true;
    }

    return &g_fl_allocator;
}

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
        s64 start_offset = (u8*)node - (u8*)fl->start;
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

}

