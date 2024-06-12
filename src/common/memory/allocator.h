#pragma once

#include "defines.h"

// #define NOVO_TRACE_ALLOC

namespace Novo {


enum class Allocator_Mode
{
    ALLOCATE,
    REALLOCATE,
    FREE,
    FREE_ALL,
};

typedef u64 Allocator_Flags;

enum Allocator_Flag : Allocator_Flags
{
    ALLOCATOR_FLAG_NONE         = 0x00,
    ALLOCATOR_FLAG_CANT_FREE    = 0x01,
    ALLOCATOR_FLAG_CANT_REALLOC = 0x02,
};

#ifdef NOVO_TRACE_ALLOC

    NAPI void report_allocator_trace();

#   define FN_ALLOCATOR(f) void * f(Allocator_Mode mode, s64 size, u64 align, s64 old_size, void* old_pointer, void* allocator_data, s64 options, const char* file, s64 line)
    typedef FN_ALLOCATOR(FN_Allocator);

#else // NOVO_TRACE_ALLOC
#   define FN_ALLOCATOR(f) void * f(Allocator_Mode mode, s64 size, u64 align, s64 old_size, void* old_pointer, void* allocator_data, s64 options)
    typedef FN_ALLOCATOR(FN_Allocator);
#endif // NOVO_TRACE_ALLOC

struct Allocator {
    FN_Allocator* fn;
    void* user_data;

    Allocator_Flags flags;
};

NAPI Allocator* c_allocator();

NAPI FN_ALLOCATOR(c_allocator_fn);

#ifdef NOVO_TRACE_ALLOC

#   define FN_ALLOCATE(f) void* f(Allocator* allocator, s64 size, const char* file, s64 line)
    typedef FN_ALLOCATE(FN_Allocate);
    NAPI FN_ALLOCATE(allocate_unaligned);

#   define FN_ALLOCATE_ALIGNED(f) void* f(Allocator* allocator, s64 size, u64 align, const char* file, s64 line)
    typedef FN_ALLOCATE_ALIGNED(FN_Allocate_Aligned);
    NAPI FN_ALLOCATE_ALIGNED(allocate_aligned);

#   define FN_FREE(f) void f(Allocator* allocator, void* ptr, const char* file, s64 line)
    typedef FN_FREE(FN_Free);
    NAPI FN_FREE(allocate_free);

#   define FN_FREE_ALL(f) void f(Allocator* allocator, const char* file, s64 line)
    typedef FN_FREE_ALL(FN_Free_All);
    NAPI FN_FREE_ALL(allocate_free_all);

#   define allocate(allocator, type) \
        (type*)(allocate_unaligned((allocator), sizeof(type), __FILE__, __LINE__))

#   define allocate_size(allocator, size, cast_type) \
        (cast_type*)(allocate_unaligned((allocator), (size), __FILE__, __LINE__))

#   define allocate_size_align(allocator, size, align, cast_type) \
        (cast_type*)(allocate_aligned((allocator), (size), (align), __FILE__, __LINE__))

#   define allocate_array(allocator, type, length) \
       (type*)(allocate_aligned((allocator), sizeof(type) * (length), alignof(type), __FILE__, __LINE__))

#   define release(allocator, ptr) \
       allocate_free((allocator), (void*)(ptr), __FILE__, __LINE__)

#   define release_all(allocator) \
       allocate_free_all((allocator), __FILE__, __LINE__)


#else // NOVO_TRACE_ALLOC

#   define FN_ALLOCATE(f) void* f(Allocator* allocator, s64 size)
    typedef FN_ALLOCATE(FN_Allocate);
    NAPI FN_ALLOCATE(allocate_unaligned);

#   define FN_ALLOCATE_ALIGNED(f) void* f(Allocator* allocator, s64 size, u64 align)
    typedef FN_ALLOCATE_ALIGNED(FN_Allocate_Aligned);
    NAPI FN_ALLOCATE_ALIGNED(allocate_aligned);

#   define FN_FREE(f) void f(Allocator* allocator, void* ptr)
    typedef FN_FREE(FN_Free);
    NAPI FN_FREE(allocate_free);

#   define FN_FREE_ALL(f) void f(Allocator* allocator)
    typedef FN_FREE_ALL(FN_Free_All);
    NAPI FN_FREE_ALL(allocate_free_all);

#   define allocate(allocator, type) \
       (type*)(allocate_aligned((allocator), sizeof(type), alignof(type)));

#   define allocate_size(allocator, size, cast_type) \
        (cast_type*)(allocate_unaligned((allocator), (size)));

#   define allocate_size_align(allocator, size, align, cast_type) \
        (cast_type*)(allocate_aligned((allocator), (size), (align)))

#   define allocate_array(allocator, type, length) \
       (type*)(allocate_aligned((allocator), sizeof(type) * (length), alignof(type)))

#   define release(allocator, ptr) \
       allocate_free((allocator), (ptr))

#   define release_all(allocator) \
       allocate_free_all((allocator))

#endif // NOVO_TRACE_ALLOC



}
