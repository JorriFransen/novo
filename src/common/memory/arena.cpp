#include "arena.h"

#include "defines.h"
#include "memory/allocator.h"
#include "nstring.h"

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
    return { arena_allocator_fn, arena, ALLOCATOR_FLAG_CANT_FREE | ALLOCATOR_FLAG_CANT_REALLOC };
}

Arena arena_create(u8* data, s64 size)
{
    Arena result;
    result.data = data;
    result.used = 0;
    result.capacity = size;
    result.max_capacity = size;
    result.flags = ARENA_FLAG_NONE;
    return result;
}

void arena_new(Arena* arena, u64 max_cap/*=NOVO_ARENA_MAX_CAP*/)
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

    assert(max_cap % sys_info.dwPageSize == 0);

    void* base = VirtualAlloc(nullptr, max_cap, MEM_RESERVE, PAGE_NOACCESS);
    assert(base);

    void* cbase = VirtualAlloc(base, size, MEM_COMMIT, PAGE_READWRITE);
    assert(cbase == base);

    arena->data = (u8*)base;
    arena->used = 0;
    arena->capacity = size;
    arena->max_capacity = max_cap;
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
    s64 aligned_size = size;
    if (!(arena->flags & ARENA_FLAG_NOALIGN)) {
        aligned_size = size + (align - 1);
    }

    void* start = arena->data + arena->used;

    if (arena->used + aligned_size > arena->capacity) {
        if (!arena_grow(arena, aligned_size)) {
            assert(false && !"Growing arena failed!");
            return nullptr;
        }
    }

    arena->used += aligned_size;

    void* result = start;

    if (!(arena->flags & ARENA_FLAG_NOALIGN)) {
        result = (void*)get_aligned((u64)start, align);
    }

    memset(result, 0, size);

    return result;
}

bool arena_grow(Arena* arena, u64 min_size)
{
    if (!(arena->flags & ARENA_FLAG_GROW)) return false;

    u64 new_cap = arena->capacity * 2;
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

        case Allocator_Mode::FREE: return nullptr;

        case Allocator_Mode::FREE_ALL: {
            arena_reset(arena);
            return nullptr;
        }
    }

    assert(false);
    return nullptr;
}

Temp_Arena temp_arena_create(Arena* arena)
{
    Temp_Arena result;
    result.arena = arena;
    result.reset_to = arena->used;
    return result;
}

void temp_arena_release(Temp_Arena ta)
{
    assert(ta.arena->used >= ta.reset_to);
    ta.arena->used = ta.reset_to;
}

N__tls Arena g_temp_arena_a;
N__tls Arena g_temp_arena_b;
N__tls Arena* g_temp_arena_next = nullptr;

Temp_Arena temp_arena(Arena* dont_use)
{
    if (!g_temp_arena_next) {
        // First use, initialize

        arena_new(&g_temp_arena_a);
        arena_new(&g_temp_arena_b);
        g_temp_arena_next = &g_temp_arena_a;
    }

    Arena* use_arena = nullptr;

    if (dont_use == &g_temp_arena_a) {
        use_arena = &g_temp_arena_b;
        g_temp_arena_next = &g_temp_arena_a;
    } else if (dont_use == &g_temp_arena_b) {
        use_arena = &g_temp_arena_a;
        g_temp_arena_next = &g_temp_arena_b;
    } else {
        use_arena = g_temp_arena_next;
        if (&g_temp_arena_a == g_temp_arena_next) {
            g_temp_arena_next = &g_temp_arena_b;
        } else {
            assert(&g_temp_arena_b == g_temp_arena_next);
            g_temp_arena_next = &g_temp_arena_a;
        }
    }

    assert(use_arena);

    return temp_arena_create(use_arena);
}

}
