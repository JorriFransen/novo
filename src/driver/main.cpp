#include <stdio.h>

#include <allocator.h>

using namespace Novo;

int main(int argc, char *argv[])
{
    int *p1 = (int *)allocate(10 * sizeof(p1));
    printf("default-aligned addr: %p\n", p1);

    int *p2 = (int *)allocate_aligned(10 * sizeof(p2), 32);
    printf("default-aligned addr: %p\n", p2);

    int *p3 = (int *)allocate_aligned(10 * sizeof(p3), 32);
    printf("default-aligned addr: %p\n", p3);

    free(p1);
    free(p2);
    free(c_allocator, p3);
    return 0;
}
