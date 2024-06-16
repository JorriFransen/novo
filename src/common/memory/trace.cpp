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

    printf("  total allocations:   %lld\n", trace->trace_alloc_records.count);

    clock_t total_alloc_time = 0;
    clock_t total_release_time = 0;

    for (s64 i = 0; i < trace->trace_alloc_records.count; i++) {
        Trace_Alloc_Record record = trace->trace_alloc_records[i];
        clock_t release_time = 0;
        if (record.release_record_index >= 0) {
            assert(record.release_record_index < trace->trace_release_records.count);
            Trace_Release_Record release_record = trace->trace_release_records[record.release_record_index];
            release_time = release_record.time;
        }
        // printf("        %s:%lld: (alloc: %f sec) (free: %f sec)\n", record.file, record.line, (double)record.time / CLOCKS_PER_SEC, (double)release_time / CLOCKS_PER_SEC);
        total_alloc_time += record.time;
        total_release_time += release_time;
    }

    printf("    total alloc time:   %f sec\n", (double)total_alloc_time / CLOCKS_PER_SEC);
    printf("    total release time: %f sec\n", (double)total_release_time / CLOCKS_PER_SEC);
    printf("    total time:         %f sec\n", (double)(total_alloc_time + total_release_time) / CLOCKS_PER_SEC);



    printf("  leaked allocations:  %lld\n", trace->trace_alloc_live_allocs.count);

    for (s64 i = 0; i < trace->trace_alloc_live_allocs.count; i++) {
        s64 record_index = trace->trace_alloc_live_allocs[i].record_index;
        Trace_Alloc_Record record = trace->trace_alloc_records[record_index];
        printf("        %s:%lld\n", record.file, record.line);
    }

    printf("===========================\n\n");
}

}

#endif // NOVO_TRACE_ALLOC
