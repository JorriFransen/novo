
#include <defines.h>
#include <memory/c_allocator.h>

#include <cassert>
#include <cstdio>
#include <memory/arena.h>

using namespace Novo;

bool test_align(Allocator* allocator);

int main(int argc, char* argv[])
{


    Arena arena;
    arena_new(&arena);
    Allocator arena_allocator = arena_allocator_create(&arena);

    Allocator* allocators[] = {
        c_allocator(),
        &arena_allocator,
    };

    bool ok = true;

    for (unsigned i = 0; i < sizeof(allocators) / sizeof(allocators[0]); i++) {
        if (!test_align(allocators[i])) {
            ok = false;
        }
    }

    arena_free(&arena);

    return ok ? 0 : 1;
}

bool test_align(Allocator* allocator)
{
    for (u64 align = 1; align <= 4096; align *= 2) {

        for (s64 i = 1; i <= 3; i++) {
            s64 size = align * i;
            printf("align: %llu, size: %lld\n", align, size);

            void* ptr = allocate_size_align(allocator, size, align, void);
            assert((u64)ptr % align == 0);

            if (!(allocator->flags & ALLOCATOR_FLAG_CANT_FREE)) release(allocator, ptr);
        }

        printf("\n");
        if (allocator->flags & ALLOCATOR_FLAG_CANT_FREE) release_all(allocator);

    }

    return true;
}

