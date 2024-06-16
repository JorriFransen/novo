#pragma once

#ifdef NOVO_TRACE_ALLOC

#include "defines.h"

#include "containers/darray.h"
#include "memory/arena.h"
#include "nstring.h"

#include <ctime>

namespace Novo {

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

struct Allocator_Trace
{
    Arena trace_alloc_arena;
    Allocator trace_alloc_allocator;
    DArray<Trace_Alloc_Record> trace_alloc_records;
    DArray<Trace_Release_Record> trace_release_records;
    DArray<Trace_Alloc_Live> trace_alloc_live_allocs;
};


#define trace_timer_start(name) clock_t name##_start = clock();

#define trace_alloc_timer_end(trace_, name_, ptr) { \
    clock_t name = clock() - name_##_start; \
    Allocator_Trace* trace = (Allocator_Trace*)(trace_); \
    s64 record_index = trace->trace_alloc_records.count; \
    darray_append(&trace->trace_alloc_records, { (ptr), file, line, name, false } ); \
    darray_append(&trace->trace_alloc_live_allocs, { (ptr), record_index }); \
}

#define trace_release_timer_end(trace_, name_, old_ptr) { \
    clock_t name = clock() - name_##_start; \
    Allocator_Trace* trace = (Allocator_Trace*)(trace_); \
    s64 record_index = -1; \
    for (s64 i = 0; i < trace->trace_alloc_live_allocs.count; i++) { \
        if (trace->trace_alloc_live_allocs[i].ptr == (old_ptr)) { \
            record_index = trace->trace_alloc_live_allocs[i].record_index; \
            darray_remove_unordered(&trace->trace_alloc_live_allocs, i); \
            break; \
        } \
    } \
    assert(record_index >= 0); \
    assert(record_index < trace->trace_alloc_records.count); \
    s64 release_record_index = trace->trace_release_records.count; \
    trace->trace_alloc_records[record_index].release_record_index = release_record_index; \
    darray_append(&trace->trace_release_records, { old_pointer, file, line,  name }); \
}

NAPI void init_allocator_trace(Allocator_Trace *trace);
NAPI void report_allocator_trace(String_Ref allocator_name, Allocator_Trace *trace);

}

#else // NOVO_TRACE_ALLOC

#define trace_timer_start(name)
#define trace_alloc_timer_end(trace, name, ptr)
#define trace_release_timer_end(trace, name, ptr)

#endif // NOVO_TRACE_ALLOC
