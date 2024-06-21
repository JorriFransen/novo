#include "freelist.h"

#include "defines.h"
#include "filesystem.h"
#include "memory/allocator.h"
#include "memory/arena.h"
#include "memory/trace.h"
#include "string_builder.h"

#include <cassert>

namespace Novo {

void freelist_init(Freelist* freelist, Arena arena)
{
    freelist->arena = arena;
    freelist->remaining = arena.capacity;

    freelist_reset(freelist);

#ifdef NOVO_TRACE_ALLOC
    init_allocator_trace(&freelist->trace);
#endif // NOVO_TRACE_ALLOC

}

void freelist_reset(Freelist* freelist)
{
    assert(freelist->arena.capacity > sizeof(Freelist_Node));
    Freelist_Node* node = (Freelist_Node*)freelist->arena.data;

    node->size = freelist->arena.capacity;
    node->next = nullptr;

    freelist->first_free = node;
    freelist->remaining = freelist->arena.capacity;
}

Freelist_Node* freelist_grow(Freelist* freelist, s64 min_increase, Freelist_Node** prev_out)
{
    if (!(freelist->arena.flags & ARENA_FLAG_GROW)) return nullptr;

    s64 old_cap = freelist->arena.capacity;

    bool grow_res = arena_grow(&freelist->arena, old_cap + min_increase);
    assert(grow_res);

    s64 increase = freelist->arena.capacity - old_cap;
    assert(increase >= min_increase);
    freelist->remaining += increase;

    Freelist_Node* last_node = freelist->first_free;
    Freelist_Node* prev_node = nullptr;
    if (last_node) {
        while (last_node->next) {
            prev_node = last_node;
            last_node = last_node->next;
        }
    }

    Freelist_Node* new_node = (Freelist_Node*)(freelist->arena.data + old_cap);
    new_node->size = increase;
    new_node->next = nullptr;

    Freelist_Node* result = nullptr;

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

void freelist_insert(Freelist* freelist, Freelist_Node* insert_after, Freelist_Node* node)
{
    if (insert_after) {
        node->next = insert_after->next;
        insert_after->next = node;
    } else {
        if (freelist->first_free) {
            node->next = freelist->first_free;
        }
        freelist->first_free = node;
    }
}

void freelist_remove(Freelist* freelist, Freelist_Node* prev, Freelist_Node* node)
{
    if (prev) {
        prev->next = node->next;
    } else {
        freelist->first_free = node->next;
    }
}

static bool is_power_of_two(p_uint_t x) { return (x & (x - 1)) == 0; };

static size_t calc_padding_with_header(p_uint_t operand, size_t align, size_t header_size)
{
    assert(is_power_of_two(align));

    p_uint_t p = operand;
    p_uint_t a = (p_uint_t)align;
    p_uint_t modulo = p & (a - 1);
    size_t padding = 0;

    if (modulo) {
        padding = a - modulo;
    }

    size_t needed_space = header_size;

    if (padding < needed_space) {
        needed_space -= padding;

        if ((needed_space & (a - 1)) != 0) {
            padding += a * (1 + (needed_space / a));
        } else {
            padding += a * (needed_space / a);
        }

    }
    return padding;
}

Freelist_Node* freelist_find_first(Freelist* freelist, size_t size, size_t align, size_t* padding_out, Freelist_Node** prev_node_out)
{
    Freelist_Node* node = freelist->first_free;
    Freelist_Node* prev_node = nullptr;

    size_t padding = 0;
    while (node) {
        padding = calc_padding_with_header((p_uint_t)node, (p_uint_t)align, sizeof(Freelist_Alloc_Header));
        size_t required_space = size + padding;
        if (node->size >= required_space) {
            break;
        }

        prev_node = node;
        node = node->next;
    }

    if (padding_out) *padding_out = padding;
    if (prev_node_out) *prev_node_out = prev_node;

    return node;
}

void* freelist_allocate(Freelist* freelist, s64 size, s64 align)
{
    if (size < sizeof(Freelist_Node)) {
        size = sizeof(Freelist_Node);
    }

    if (align < sizeof(size_t)) {
        align = sizeof(size_t);
    }

    size_t padding = 0;

    Freelist_Node* prev;
    Freelist_Node* node = freelist_find_first(freelist, size, align, &padding, &prev);

    if (node == nullptr) {

        freelist_grow(freelist, size, nullptr);
        node = freelist_find_first(freelist, size, align, &padding, &prev);

        if (node == nullptr) {
            assert(false && "Freelist out of memory");
            return nullptr;
        }
    }

    size_t alignment_padding = padding - sizeof(Freelist_Alloc_Header);
    size_t required_space = size + padding;
    size_t remaining = node->size - required_space;

    if (remaining >= sizeof(Freelist_Node)) {
        Freelist_Node* new_node = (Freelist_Node*)((u8*)node + required_space);
        new_node->size = remaining;
        freelist_insert(freelist, node, new_node);
    } else {
        required_space += remaining;
    }

    freelist_remove(freelist, prev, node);

    Freelist_Alloc_Header* alloc_header = (Freelist_Alloc_Header*)((u8*)node + alignment_padding);
    alloc_header->size = required_space;
    alloc_header->padding = alignment_padding;

    freelist->remaining -= required_space;

    void* result = (void*)((u8*)alloc_header + sizeof(Freelist_Alloc_Header));

    memset(result, 0, size);

    return result;
}

void freelist_release(Freelist* freelist, void* ptr)
{
    assert(ptr);

    Freelist_Alloc_Header* header_pointer = (Freelist_Alloc_Header*)((u8*)ptr - sizeof(Freelist_Alloc_Header));
    Freelist_Alloc_Header header = *header_pointer;

    Freelist_Node* new_node = (Freelist_Node*)((u8*)header_pointer - header.padding);
    new_node->size = header.size;
    new_node->next = nullptr;

    freelist->remaining += new_node->size;

    Freelist_Node* node = freelist->first_free;
    Freelist_Node* prev_node = nullptr;

    if (!node) {
        freelist->first_free = new_node;
        return;
    }

    while (node) {
        if (ptr < node) {
            freelist_insert(freelist, prev_node, new_node);
            break;
        }

        prev_node = node;
        node = node->next;
    }

    if (new_node->next && (u8*)new_node + new_node->size == (u8*)new_node->next) {
        new_node->size += new_node->next->size;
        freelist_remove(freelist, new_node, new_node->next);
    }

    if (prev_node && (u8*)prev_node + prev_node->size == (u8*)new_node) {
        assert(prev_node->next == new_node);
        prev_node->size += new_node->size;
        freelist_remove(freelist, prev_node, new_node);
    }
}

FN_ALLOCATOR(fl_allocator_fn)
{
    Freelist* freelist = (Freelist*)allocator_data;

    switch (mode) {
        case Allocator_Mode::ALLOCATE: {

            trace_timer_start(alloc_time);
            void* result = freelist_allocate(freelist, size, align);
            trace_alloc_timer_end(&freelist->trace, alloc_time, result, size);

            return result;
        }

        case Allocator_Mode::REALLOCATE: assert(false); break;

        case Allocator_Mode::FREE: {

            trace_timer_start(release_time);
            freelist_release(freelist, old_pointer);
            trace_release_timer_end(&freelist->trace, release_time, old_pointer);

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
        g_fl_allocator = { fl_allocator_fn, &g_freelist, ALLOCATOR_FLAG_NONE };
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

    Freelist_Node* node = fl->first_free;
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


