
#include <allocator.h>

#include <cassert>
#include <cstdio>

using namespace Novo;

bool test_align(FN_Allocator allocator)
{
    for (u64 align = 1; align <= 4096; align *= 2) {

        for (s64 i = 1; i <= 3; i++) {
            s64 size = align * i;
            printf("align: %llu, size: %lld\n", align, size);

            void *ptr = allocate_aligned(size, align);
            assert((u64)ptr % align == 0);
            free(ptr);
        }

        printf("\n");

    }

    return true;
}

int main(int argc, char *argv[])
{
    if (!test_align(c_allocator)) {
        return 1;
    }

    return 0;
}
