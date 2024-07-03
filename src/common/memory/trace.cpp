#include "trace.h"

#ifdef NOVO_TRACE_ALLOC

#include <stdio.h>

namespace Novo {

void init_allocator_trace(Allocator_Trace *trace)
{
    arena_new(&trace->trace_alloc_arena);
    trace->trace_alloc_allocator = arena_allocator_create(&trace->trace_alloc_arena);
    darray_init(&trace->trace_alloc_allocator, &trace->trace_alloc_records);
    darray_init(&trace->trace_alloc_allocator, &trace->trace_release_records);
    darray_init(&trace->trace_alloc_allocator, &trace->trace_alloc_live_allocs);
}

void report_allocator_trace(String_Ref name, Allocator_Trace *trace)
{
    printf("\n\n");
    printf("===========================\n");
    printf("%.*s trace report:\n", (int)name.length, name.data);

    clock_t total_alloc_time = 0;
    clock_t total_extend_time = 0;
    clock_t total_realloc_time = 0;
    clock_t total_release_time = 0;

    int alloc_count = 0;
    int extend_count = 0;
    int realloc_count = 0;

    for (s64 i = 0; i < trace->trace_alloc_records.count; i++) {
        Trace_Alloc_Record record = trace->trace_alloc_records[i];

        clock_t release_time = 0;
        if (record.release_record_index >= 0) {
            assert(record.release_record_index < trace->trace_release_records.count);
            Trace_Release_Record release_record = trace->trace_release_records[record.release_record_index];
            release_time = release_record.time;
        }

        if (record.flags & TRACE_ALLOC_FLAG_EXTEND) {
            total_extend_time += record.time;
            extend_count += 1;
        } else if (record.flags & TRACE_ALLOC_FLAG_REALLOC) {
            total_realloc_time += record.time;
            realloc_count += 1;
        } else {
            total_alloc_time += record.time;
            alloc_count += 1;
        }

        // printf("        %s:%lld: (alloc: %f sec) (free: %f sec)\n", record.file, record.line, (double)record.time / CLOCKS_PER_SEC, (double)release_time / CLOCKS_PER_SEC);
        total_release_time += release_time;
    }

    printf("  alloc count:          %d\n", alloc_count);
    printf("  extension count:      %d\n", extend_count);
    printf("  realloc count:        %d\n", realloc_count);


    printf("    total alloc time:   %f sec\n", (double)total_alloc_time / CLOCKS_PER_SEC);
    printf("    total extend time:  %f sec\n", (double)total_extend_time / CLOCKS_PER_SEC);
    printf("    total realloc time: %f sec\n", (double)total_realloc_time / CLOCKS_PER_SEC);
    printf("    total release time: %f sec\n", (double)total_release_time / CLOCKS_PER_SEC);
    printf("    total time:         %f sec\n", (double)(total_alloc_time + total_release_time) / CLOCKS_PER_SEC);



    printf("  leaked allocations:   %lld\n", trace->trace_alloc_live_allocs.count);

    for (s64 i = 0; i < trace->trace_alloc_live_allocs.count; i++) {
        s64 record_index = trace->trace_alloc_live_allocs[i].record_index;
        Trace_Alloc_Record record = trace->trace_alloc_records[record_index];
        printf("        %s:%lld\n", record.file, record.line);
    }

    printf("===========================\n\n");
}

}

#endif // NOVO_TRACE_ALLOC
