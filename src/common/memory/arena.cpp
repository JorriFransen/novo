#include "arena.h"
#include "defines.h"
#include "memory/allocator.h"

#include <cassert>

#include <unistd.h>
#include <sys/mman.h>

namespace Novo {

Allocator arena_allocator_create(Arena* arena)
{
    arena_create(arena);
    return { arena_allocator_fn, arena, ALLOCATOR_FLAG_CANT_FREE | ALLOCATOR_FLAG_CANT_REALLOC };
}

void arena_create(Arena* arena)
{
    s64 size = sysconf(_SC_PAGE_SIZE);
    assert(size != -1);

    void* p = mmap(nullptr, NOVO_ARENA_MMAP_SIZE, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    assert(p != MAP_FAILED);

    int ok = mprotect(p, size, PROT_READ | PROT_WRITE);
    assert(ok != -1);

    arena->data = (u8*)p;
    arena->used = 0;
    arena->capacity = size;
    arena->flags = ARENA_FLAG_GROW;
}

void arena_free(Arena* arena)
{
    if (!(arena->flags & ARENA_FLAG_GROW)) {
        return;
    }

    int ok = munmap(arena->data, NOVO_ARENA_MMAP_SIZE);
    assert(ok != -1);

    arena->data = nullptr;
    arena->used = -1;
    arena->capacity = -1;
}

void* arena_alloc(Arena* arena, s64 size, s64 align)
{
    if (!(arena->flags & ARENA_FLAG_NOALIGN)) {
        size = size + (align - 1);
    }

    void* p = arena->data + arena->used;

    if (arena->used + size > arena->capacity) {
        if (!arena_grow(arena, size)) {
            assert(false && !"Growing arena failed!");
            return nullptr;
        }
    }

    arena->used += size;

    return (void*)get_aligned((u64)p, align);
}

bool arena_grow(Arena* arena, s64 min_size)
{
    if (!(arena->flags & ARENA_FLAG_GROW)) return false;

    s64 new_cap = arena->capacity * 2;
    while (new_cap < min_size) new_cap *= 2;

    int ok = mprotect(arena->data + arena->capacity, new_cap - arena->capacity, PROT_READ | PROT_WRITE);
    assert(ok != -1);

    arena->capacity = new_cap;

    return true;
}

void arena_reset(Arena* arena)
{
    arena->used = 0;
}

NAPI FN_ALLOCATOR(arena_allocator_fn)
{
    Arena* arena = (Arena*)allocator_data;

    switch (mode) {
        case Allocator_Mode::ALLOCATE: {
            return arena_alloc(arena, size, align);
        }

        case Allocator_Mode::REALLOCATE: assert(false); break;
        case Allocator_Mode::FREE: assert(false); break;

        case Allocator_Mode::FREE_ALL: {
            arena_reset(arena);
            return nullptr;
        }
    }

    assert(false);
    return nullptr;
}

}
