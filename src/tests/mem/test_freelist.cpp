
#include "memory/allocator.h"
#include "memory/freelist.h"

#include <cassert>

using namespace Novo;

int main() {

    Allocator* flalloc = fl_allocator();
    Freelist* fl = (Freelist*)flalloc->user_data;

    Freelist_Header* initial_node = fl->first_free;

    u64* p1 = allocate(flalloc, u64);
    assert(fl->first_free != initial_node);

    u64* p2 = allocate_size(flalloc, 8, u64);

    assert(p1 != p2);

    release(flalloc, p1);
    assert(fl->first_free == initial_node);
    assert(fl->first_free->next);

    u64* p3 = allocate_size(flalloc, 8, u64);
    assert(p1 == p3);
    assert(fl->first_free != initial_node);

    release(flalloc, p2);
    release(flalloc, p3);
    return 0;
}
