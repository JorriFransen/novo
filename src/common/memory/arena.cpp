#include "arena.h"
#include "defines.h"
#include "memory/allocator.h"

#include <cassert>

#if NPLATFORM_LINUX

#include <unistd.h>
#include <sys/mman.h>

#elif NPLATFORM_WINDOWS // NPLATFORM_LINUX

#include "Windows.h"
#include "Memoryapi.h"

#else // NPLATFORM_LINUX
STATIC_ASSERT(false, "Unsupported platform")
#endif // NPLATFORM_LINUX

namespace Novo {

Allocator arena_allocator_create(Arena* arena)
{
    arena_new(arena);
    return { arena_allocator_fn, arena, ALLOCATOR_FLAG_CANT_FREE | ALLOCATOR_FLAG_CANT_REALLOC };
}

void arena_new(Arena* arena, s64 max_cap/*=NOVO_ARENA_MAX_CAP*/)
{
#if NPLATFORM_LINUX
    s64 size = sysconf(_SC_PAGE_SIZE);
    assert(size != -1);

    void* p = mmap(nullptr, max_cap, PROT_NONE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
    assert(p != MAP_FAILED);

    int ok = mprotect(p, size, PROT_READ | PROT_WRITE);
    assert(ok != -1);

    arena->data = (u8*)p;
    arena->used = 0;
    arena->capacity = size;
    arena->max_capacity = max_cap;
    arena->flags = ARENA_FLAG_GROW;
#else
    STATIC_ASSERT(NPLATFORM_WINDOWS);

    SYSTEM_INFO sys_info;
    GetSystemInfo(&sys_info);
    s64 size = sys_info.dwPageSize;

    void* base = VirtualAlloc(nullptr, max_size, MEM_RESERVE, PAGE_NOACCESS);
    assert(base);

    void* cbase = VirtualAlloc(base, size, MEM_COMMIT, PAGE_READWRITE);
    assert(cbase == base);

    arena->data = (u8*)base;
    arena->used = 0;
    arena->capacity = size;
    arena->flags = ARENA_FLAG_GROW;
#endif
}

void arena_free(Arena* arena)
{
    if (!(arena->flags & ARENA_FLAG_GROW)) {
        return;
    }

#if NPLATFORM_LINUX

    int ok = munmap(arena->data, arena->max_capacity);
    assert(ok != -1);

#else
    STATIC_ASSERT(NPLATFORM_WINDOWS);

    BOOL ok = VirtualFree(arena->data, 0, MEM_RELEASE);
    assert(ok);
#endif

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

    if (!(arena->flags & ARENA_FLAG_NOALIGN)) {
        return (void*)get_aligned((u64)p, align);
    } else {
        return p;
    }
}

bool arena_grow(Arena* arena, s64 min_size)
{
    if (!(arena->flags & ARENA_FLAG_GROW)) return false;

    s64 new_cap = arena->capacity * 2;
    while (new_cap < min_size) new_cap *= 2;

    assert(new_cap <= NOVO_ARENA_MAX_CAP);

#if NPLATFORM_LINUX

    int ok = mprotect(arena->data + arena->capacity, new_cap - arena->capacity, PROT_READ | PROT_WRITE);
    assert(ok != -1);

    arena->capacity = new_cap;

    return true;
#else
    STATIC_ASSERT(NPLATFORM_WINDOWS);

    void* cbase = VirtualAlloc(arena->data + arena->capacity, new_cap - arena->capacity, MEM_COMMIT, PAGE_READWRITE);
    assert(cbase == arena->data + arena->capacity);

    arena->capacity = new_cap;

    return true;
#endif
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
