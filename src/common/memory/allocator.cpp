#include "allocator.h"
#include <ctime>

#ifdef NOVO_TRACE_ALLOC
#   include "containers/darray.h"
#   include "memory/arena.h"

#   include <cstdio>
#endif // NOVO_TRACE_ALLOC

#include <cassert>
#include <cstdlib>
#include <cstring>

namespace Novo {

#ifdef NOVO_TRACE_ALLOC

    struct Trace_Alloc_Record
    {
        void* ptr;
        const char *file;
        s64 line;
        clock_t time;
        s64 release_record_index;
    };

    struct Trace_Release_Record
    {
        void* ptr;
        const char *file;
        s64 line;
        clock_t time;
    };

    struct Trace_Alloc_Live
    {
        void* ptr;
        s64 record_index;
    };

    Arena trace_alloc_arena;
    Allocator trace_alloc_allocator;
    DArray<Trace_Alloc_Record> trace_alloc_records;
    DArray<Trace_Release_Record> trace_release_records;
    DArray<Trace_Alloc_Live> trace_alloc_live_allocs;


    void report_allocator_trace()
    {
        printf("\n\n");
        printf("===========================\n");
        printf("c_allocator() trace report:\n");

        printf("  total allocations:   %lld\n", trace_alloc_records.count);

        clock_t total_alloc_time = 0;
        clock_t total_release_time = 0;

        for (s64 i = 0; i < trace_alloc_records.count; i++) {
            Trace_Alloc_Record record = trace_alloc_records[i];
            clock_t release_time = 0;
            if (record.release_record_index >= 0) {
                assert(record.release_record_index < trace_release_records.count);
                Trace_Release_Record release_record = trace_release_records[record.release_record_index];
                release_time = release_record.time;
            }
            // printf("        %s:%lld: (alloc: %f sec) (free: %f sec)\n", record.file, record.line, (double)record.time / CLOCKS_PER_SEC, (double)release_time / CLOCKS_PER_SEC);
            total_alloc_time += record.time;
            total_release_time += release_time;
        }

        printf("    total alloc time:   %f sec\n", (double)total_alloc_time / CLOCKS_PER_SEC);
        printf("    total release time: %f sec\n", (double)total_release_time / CLOCKS_PER_SEC);
        printf("    total time:         %f sec\n", (double)(total_alloc_time + total_release_time) / CLOCKS_PER_SEC);



        printf("  leaked allocations:  %lld\n", trace_alloc_live_allocs.count);

        for (s64 i = 0; i < trace_alloc_live_allocs.count; i++) {
            s64 record_index = trace_alloc_live_allocs[i].record_index;
            Trace_Alloc_Record record = trace_alloc_records[record_index];
            printf("        %s:%lld\n", record.file, record.line);
        }

        printf("===========================\n\n");
    }

#   define trace_timer_start(name) clock_t name##_start = clock();
#   define trace_timer_end(name) clock_t name = clock() - name##_start;


#else // NOVO_TRACE_ALLOC
#   define trace_timer_start(name)
#   define trace_timer_end(name)
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
            darray_init(&trace_alloc_allocator, &trace_release_records);
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
        trace_timer_start(malloc_time);
        u8* mem = (u8*)malloc(actual_size);
        trace_timer_end(malloc_time);
        void **ptr = (void**)get_aligned((u64)mem + sizeof(void*), align);

        // Store the pointer returned by malloc
        ptr[-1] = mem;

        memset(ptr, 0, size);

        #ifdef NOVO_TRACE_ALLOC
            s64 record_index = trace_alloc_records.count;
            darray_append(&trace_alloc_records, { ptr, file, line, malloc_time, false } );
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
        trace_timer_start(release_time);
        ::free(_old);
        trace_timer_end(release_time);

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
            s64 release_record_index = trace_release_records.count;
            trace_alloc_records[record_index].release_record_index = release_record_index;
            darray_append(&trace_release_records, { old_pointer, file, line,  release_time });
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
