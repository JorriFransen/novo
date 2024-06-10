#include "allocator.h"
#include "containers/darray.h"

#ifdef NOVO_TRACE_ALLOC
#   include "memory/arena.h"
#endif // NOVO_TRACE_ALLOC

#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

namespace Novo {

#ifdef NOVO_TRACE_ALLOC

    struct Trace_Alloc_Record
    {
        void* ptr;
        const char *file;
        s64 line;
        bool freed;
    };

    struct Trace_Alloc_Live
    {
        void* ptr;
        s64 record_index;
    };

    Arena trace_alloc_arena;
    Allocator trace_alloc_allocator;
    DArray<Trace_Alloc_Record> trace_alloc_records;
    DArray<Trace_Alloc_Live> trace_alloc_live_allocs;


    void report_allocator_trace()
    {
        printf("\n\n");
        printf("===========================\n");
        printf("c_allocator() trace report:\n");

        printf("  %4lld total allocations\n", trace_alloc_records.count);
        printf("  %4lld leaked allocations:\n", trace_alloc_live_allocs.count);

        for (s64 i = 0; i < trace_alloc_live_allocs.count; i++) {
            s64 record_index = trace_alloc_live_allocs[i].record_index;
            Trace_Alloc_Record record = trace_alloc_records[record_index];
            printf("        %s:%lld\n", record.file, record.line);
        }

        printf("===========================\n\n");
    }

#endif // NOVO_TRACE_ALLOC


bool g_c_allocator_initialized = false;
Allocator g_c_allocator;

Allocator* c_allocator()
{
    if (!g_c_allocator_initialized) {
        g_c_allocator = { c_allocator_fn, nullptr, ALLOCATOR_FLAG_CANT_REALLOC };
        g_c_allocator_initialized = true;

        #ifdef NOVO_TRACE_ALLOC
            arena_new(&trace_alloc_arena);
            trace_alloc_allocator = arena_allocator_create(&trace_alloc_arena);
            darray_init(&trace_alloc_allocator, &trace_alloc_records);
            darray_init(&trace_alloc_allocator, &trace_alloc_live_allocs);
        #endif // NOVO_TRACE_ALLOC
    }

    return &g_c_allocator;
}

FN_ALLOCATOR(c_allocator_fn)
{
    switch (mode) {

    case Allocator_Mode::ALLOCATE: {
        s64 actual_size = size + (align - 1) + sizeof(void*);
        u8* mem = (u8*)malloc(actual_size);
        void **ptr = (void**)get_aligned((u64)mem + sizeof(void*), align);

        // Store the pointer returned by malloc
        ptr[-1] = mem;

        memset(ptr, 0, size);

        #ifdef NOVO_TRACE_ALLOC
            s64 record_index = trace_alloc_records.count;
            darray_append(&trace_alloc_records, { ptr, file, line, false } );
            darray_append(&trace_alloc_live_allocs, { ptr, record_index });
        #endif // NOVO_TRACE_ALLOC
        return ptr;
    }

    case Allocator_Mode::REALLOCATE: {
        assert(false && "c_allocator does not support reallocation\n");
        break;
    }

    case Allocator_Mode::FREE: {
        auto old = (void**)old_pointer;
        auto _old = old[-1];
        ::free(_old);

        #ifdef NOVO_TRACE_ALLOC
            s64 record_index = -1;
            for (s64 i = 0; i < trace_alloc_live_allocs.count; i++) {
                if (trace_alloc_live_allocs[i].ptr == old_pointer) {
                    record_index = trace_alloc_live_allocs[i].record_index;
                    darray_remove_unordered(&trace_alloc_live_allocs, i);
                    break;
                }
            }
            assert(record_index >= 0);
            assert(record_index < trace_alloc_records.count);
            trace_alloc_records[record_index].freed = true;
        #endif // NOVO_TRACE_ALLOC
        break;
    }

    case Allocator_Mode::FREE_ALL:
        assert(false && "c_allocator does not support free all\n");
        break;
    }

    return nullptr;
}

FN_ALLOCATE(allocate_unaligned)
{
#ifdef NOVO_TRACE_ALLOC
    auto ptr = allocator->fn(Allocator_Mode::ALLOCATE, size, 1, 0, nullptr, allocator->user_data, 0, file, line);
#else // NOVO_TRACE_ALLOC
    auto ptr = allocator->fn(Allocator_Mode::ALLOCATE, size, 1, 0, nullptr, allocator->user_data, 0);
#endif // NOVO_TRACE_ALLOC
    return ptr;
}

FN_ALLOCATE_ALIGNED(allocate_aligned)
{
#ifdef NOVO_TRACE_ALLOC
    auto ptr = allocator->fn(Allocator_Mode::ALLOCATE, size, align, 0, nullptr, allocator->user_data, 0, file, line);
#else // NOVO_TRACE_ALLOC
    auto ptr = allocator->fn(Allocator_Mode::ALLOCATE, size, align, 0, nullptr, allocator->user_data, 0);
#endif // NOVO_TRACE_ALLOC
    return ptr;
}

FN_FREE(allocate_free)
{
#ifdef NOVO_TRACE_ALLOC
    allocator->fn(Allocator_Mode::FREE, 0, 0, 0, ptr, allocator->user_data, 0, file, line);
#else // NOVO_TRACE_ALLOC
    allocator->fn(Allocator_Mode::FREE, 0, 0, 0, ptr, allocator->user_data, 0);
#endif // NOVO_TRACE_ALLOC
}

FN_FREE_ALL(allocate_free_all)
{
#ifdef NOVO_TRACE_ALLOC
    allocator->fn(Allocator_Mode::FREE_ALL, 0, 0, 0, nullptr, allocator->user_data, 0, file, line);
#else // NOVO_TRACE_ALLOC
    allocator->fn(Allocator_Mode::FREE_ALL, 0, 0, 0, nullptr, allocator->user_data, 0);
#endif // NOVO_TRACE_ALLOC
}

}
