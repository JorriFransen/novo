
#include "memory/allocator.h"
#include "memory/freelist.h"

#include <cassert>
#include <cstdint>

using namespace Novo;

void alloc_free_one() {

    Allocator* flalloc = fl_allocator();
    Freelist* fl = (Freelist*)flalloc->user_data;

    s64 initial_size = fl->arena.capacity;
    void* memory_start = fl->first_free;
    void* memory_end = (u8*)memory_start + fl->arena.capacity;

    assert(initial_size == fl->remaining);

    const s64 alloc_size = 64;

    void* memory = freelist_allocate(fl, alloc_size, 1);
    assert(memory);
    assert(fl->remaining == initial_size - (alloc_size + sizeof(Freelist_Alloc_Header)));
    assert(fl->first_free != memory_start);
    assert(memory >= memory_start);
    assert(memory < memory_end);

    release(flalloc, memory);

    assert(fl->remaining == fl->arena.capacity);
    assert(fl->first_free == memory_start);

    freelist_reset(fl);
}

void alloc_free_multi() {

    Allocator* flalloc = fl_allocator();
    Freelist* fl = (Freelist*)flalloc->user_data;

    s64 initial_size = fl->arena.capacity;
    void* memory_start = fl->first_free;
    void* memory_end = (u8*)memory_start + fl->arena.capacity;

    assert(initial_size == fl->remaining);

    const s64 alloc_size = 8;


    void* memory1 = freelist_allocate(fl, alloc_size, 1);
    assert(memory1);
    assert(fl->remaining == initial_size - (alloc_size + sizeof(Freelist_Alloc_Header)));
    assert(fl->first_free != memory_start);
    assert(memory1 >= memory_start);
    assert(memory1 < memory_end);

    void* memory2 = freelist_allocate(fl, alloc_size, 1);
    assert(memory2);
    assert(fl->remaining == initial_size - ((alloc_size + sizeof(Freelist_Alloc_Header)) * 2));
    assert(fl->first_free != memory_start);
    assert(memory2 >= memory_start);
    assert(memory2 < memory_end);

    void* memory3 = freelist_allocate(fl, alloc_size, 1);
    assert(memory3);
    assert(fl->remaining == initial_size - ((alloc_size + sizeof(Freelist_Alloc_Header)) * 3));
    assert(fl->first_free != memory_start);
    assert(memory3 >= memory_start);
    assert(memory3 < memory_end);


    freelist_release(fl, memory2);
    assert(fl->remaining == initial_size - ((alloc_size + sizeof(Freelist_Alloc_Header)) * 2));


    void* memory4 = freelist_allocate(fl, alloc_size, 1);
    assert(memory4);
    assert(fl->remaining == initial_size - ((alloc_size + sizeof(Freelist_Alloc_Header)) * 3));
    assert(fl->first_free != memory_start);
    assert(memory4 >= memory_start);
    assert(memory4 < memory_end);

    void* memory5 = freelist_allocate(fl, alloc_size, 1);
    assert(memory5);
    assert(fl->remaining == initial_size - ((alloc_size + sizeof(Freelist_Alloc_Header)) * 4));
    assert(fl->first_free != memory_start);
    assert(memory5 >= memory_start);
    assert(memory5 < memory_end);

    void* memory6 = freelist_allocate(fl, alloc_size, 1);
    assert(memory6);
    assert(fl->remaining == initial_size - ((alloc_size + sizeof(Freelist_Alloc_Header)) * 5));
    assert(fl->first_free != memory_start);
    assert(memory6 >= memory_start);
    assert(memory6 < memory_end);

    freelist_release(fl, memory1);
    assert(fl->remaining == initial_size - ((alloc_size + sizeof(Freelist_Alloc_Header)) * 4));

    assert(fl->first_free == memory_start);
    assert(fl->first_free->next);

    freelist_release(fl, memory4);
    assert(fl->remaining == initial_size - ((alloc_size + sizeof(Freelist_Alloc_Header)) * 3));

    freelist_release(fl, memory5);
    assert(fl->remaining == initial_size - ((alloc_size + sizeof(Freelist_Alloc_Header)) * 2));

    freelist_release(fl, memory6);
    assert(fl->remaining == initial_size - ((alloc_size + sizeof(Freelist_Alloc_Header))));

    freelist_release(fl, memory3);
    assert(fl->remaining == initial_size);

    assert(fl->first_free->next == nullptr);

    assert(fl->first_free == memory_start);

    freelist_reset(fl);
}

void alloc_free_multi_size() {

    Allocator* flalloc = fl_allocator();
    Freelist* fl = (Freelist*)flalloc->user_data;

    s64 initial_size = fl->arena.capacity;
    void* memory_start = fl->first_free;
    void* memory_end = (u8*)memory_start + fl->arena.capacity;

    assert(initial_size == fl->remaining);

    const s64 alloc_size1 = 64;
    const s64 alloc_size2 = 32;
    const s64 alloc_size3 = 128;
    const s64 alloc_size4 = 16;
    const s64 alloc_size5 = 8;
    const s64 alloc_size6 = 256;


    void* memory1 = freelist_allocate(fl, alloc_size1, 1);
    assert(memory1);
    assert(fl->remaining == initial_size - (alloc_size1 + sizeof(Freelist_Alloc_Header)));
    assert(fl->first_free != memory_start);
    assert(memory1 >= memory_start);
    assert(memory1 < memory_end);

    void* memory2 = freelist_allocate(fl, alloc_size2, 1);
    assert(memory2);
    assert(fl->remaining == initial_size - ((alloc_size1 + alloc_size2) + (sizeof(Freelist_Alloc_Header) * 2)));
    assert(fl->first_free != memory_start);
    assert(memory2 >= memory_start);
    assert(memory2 < memory_end);

    void* memory3 = freelist_allocate(fl, alloc_size3, 1);
    assert(memory3);
    assert(fl->remaining == initial_size - ((alloc_size1 + alloc_size2 + alloc_size3) + (sizeof(Freelist_Alloc_Header) * 3)));
    assert(fl->first_free != memory_start);
    assert(memory3 >= memory_start);
    assert(memory3 < memory_end);


    freelist_release(fl, memory2);
    assert(fl->remaining == initial_size - ((alloc_size1 + alloc_size3) + (sizeof(Freelist_Alloc_Header) * 2)));


    void* memory4 = freelist_allocate(fl, alloc_size4, 1);
    assert(memory4);
    assert(fl->remaining == initial_size - ((alloc_size1 + alloc_size3 + alloc_size4) + (sizeof(Freelist_Alloc_Header) * 3)));
    assert(fl->first_free != memory_start);
    assert(memory4 >= memory_start);
    assert(memory4 < memory_end);

    void* memory5 = freelist_allocate(fl, alloc_size5, 1);
    assert(memory5);
    assert(fl->remaining == initial_size - ((alloc_size1 + alloc_size3 + alloc_size4 + alloc_size5) + (sizeof(Freelist_Alloc_Header) * 4)));
    assert(fl->first_free != memory_start);
    assert(memory5 >= memory_start);
    assert(memory5 < memory_end);

    void* memory6 = freelist_allocate(fl, alloc_size6, 1);
    assert(memory6);
    assert(fl->remaining == initial_size - ((alloc_size1 + alloc_size3 + alloc_size4 + alloc_size5 + alloc_size6) + (sizeof(Freelist_Alloc_Header) * 5)));
    assert(fl->first_free != memory_start);
    assert(memory6 >= memory_start);
    assert(memory6 < memory_end);

    freelist_release(fl, memory1);
    assert(fl->remaining == initial_size - ((alloc_size3 + alloc_size4 + alloc_size5 + alloc_size6) + (sizeof(Freelist_Alloc_Header) * 4)));

    assert(fl->first_free == memory_start);
    assert(fl->first_free->next);

    freelist_release(fl, memory4);
    assert(fl->remaining == initial_size - ((alloc_size3 + alloc_size5 + alloc_size6) + (sizeof(Freelist_Alloc_Header) * 3)));

    freelist_release(fl, memory5);
    assert(fl->remaining == initial_size - ((alloc_size3 + alloc_size6) + (sizeof(Freelist_Alloc_Header) * 2)));

    freelist_release(fl, memory6);
    assert(fl->remaining == initial_size - ((alloc_size3) + (sizeof(Freelist_Alloc_Header))));

    freelist_release(fl, memory3);
    assert(fl->remaining == initial_size);

    assert(fl->first_free->next == nullptr);

    assert(fl->first_free == memory_start);

    freelist_reset(fl);
}

void alloc_aligned() {

    Allocator* flalloc = fl_allocator();
    Freelist* fl = (Freelist*)flalloc->user_data;

    s64 initial_size = fl->arena.capacity;
    void* memory_start = fl->first_free;
    void* memory_end = (u8*)memory_start + fl->arena.capacity;

    assert(initial_size == fl->remaining);

#define is_aligned(POINTER, BYTE_COUNT) \
    (((uintptr_t)(const void *)(POINTER)) % (BYTE_COUNT) == 0)

    void *memory1;
    void *memory2;

    s64 old_size;

    {
        const s64 alloc_size = 8;
        const s64 alignment = 512;

        old_size = fl->remaining;

        assert(alignment > sizeof(Freelist_Alloc_Header));

        memory1 = freelist_allocate(fl, alloc_size, alignment);
        assert(memory1);
        assert(is_aligned(memory1, alignment));
        assert(fl->remaining <= old_size - (alloc_size + (alignment - 1))); // this assumes a non growing arena
        assert(fl->first_free != memory_start);
        assert(memory1 >= memory_start);
        assert(memory1 < memory_end);

    }

    {
        const s64 alloc_size = 512;
        const s64 alignment = 8;

        old_size = fl->remaining;

        assert(alignment <= sizeof(Freelist_Alloc_Header));

        memory2 = freelist_allocate(fl, alloc_size, alignment);
        assert(memory2);
        assert(is_aligned(memory2, alignment));
        assert(fl->remaining <= old_size - (alloc_size + (alignment - 1))); // this assumes a non growing arena
        assert(fl->first_free != memory_start);
        assert(memory2 >= memory_start);
        assert(memory2 < memory_end);
    }

    freelist_release(fl, memory1);
    freelist_release(fl, memory2);


    assert(fl->remaining == fl->arena.capacity);
    assert(fl->first_free == memory_start);

    freelist_reset(fl);

#undef is_aligned
}

int main() {

    alloc_free_one();
    alloc_free_multi();
    alloc_free_multi_size();

    alloc_aligned();

    return 0;
}
