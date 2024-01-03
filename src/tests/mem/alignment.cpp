
#include <memory/allocator.h>
#include <memory/linear_allocator.h>

#include <cassert>
#include <cstdio>

using namespace Novo;

struct Allocator_Info
{
    Allocator *allocator;
    bool can_free;
};

bool test_align(Allocator_Info info);

int main(int argc, char *argv[])
{

    Linear_Allocator linear_allocator_data;
    Allocator linear_allocator = linear_allocator_create(&linear_allocator_data, c_allocator(), 4096 * 2 * 6);


    Allocator_Info allocators[] = {
        { c_allocator(), true },
        { &linear_allocator, false },
    };

    bool ok = true;

    for (unsigned i = 0; i < sizeof(allocators) / sizeof(allocators[0]); i++) {
        if (!test_align(allocators[i])) {
            ok = false;
        }
    }

    return ok ? 0 : 1;
}

bool test_align(Allocator_Info info)
{
    for (u64 align = 1; align <= 4096; align *= 2) {

        for (s64 i = 1; i <= 3; i++) {
            s64 size = align * i;
            printf("align: %llu, size: %lld\n", align, size);

            void *ptr = allocate_aligned(info.allocator, size, align);
            assert((u64)ptr % align == 0);

            if (info.can_free) free(info.allocator, ptr);
        }

        printf("\n");
        if (!info.can_free) free_all(info.allocator);

    }

    return true;
}

