#pragma once

#include "defines.h"

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

#ifdef NOVO_TRACE_ALLOC

#   define FN_ALLOCATE_UNALIGNED(f) void* f(Allocator* allocator, s64 size, const char* file, s64 line)
    typedef FN_ALLOCATE_UNALIGNED(FN_Allocate);
    NAPI FN_ALLOCATE_UNALIGNED(allocate_unaligned);

#   define FN_ALLOCATE_ALIGNED(f) void* f(Allocator* allocator, s64 size, s64 align, const char* file, s64 line)
    typedef FN_ALLOCATE_ALIGNED(FN_Allocate_Aligned);
    NAPI FN_ALLOCATE_ALIGNED(allocate_aligned);

#   define FN_REALLOCATE_UNALIGNED(f) void* f(Allocator* allocator, void* old_pointer, s64 new_size, const char* file, s64 line)
    typedef FN_REALLOCATE_UNALIGNED(FN_Reallocate_Unaligned);
    NAPI FN_REALLOCATE_UNALIGNED(reallocate_unaligned);

#   define FN_REALLOCATE_ALIGNED(f) void* f(Allocator* allocator, void* old_pointer, s64 new_size, s64 align, const char* file, s64 line)
    typedef FN_REALLOCATE_ALIGNED(FN_Reallocate_Analigned);
    NAPI FN_REALLOCATE_ALIGNED(reallocate_aligned);

#   define FN_FREE(f) void f(Allocator* allocator, void* ptr, const char* file, s64 line)
    typedef FN_FREE(FN_Free);
    NAPI FN_FREE(allocate_free);

#   define FN_FREE_ALL(f) void f(Allocator* allocator, const char* file, s64 line)
    typedef FN_FREE_ALL(FN_Free_All);
    NAPI FN_FREE_ALL(allocate_free_all);

#   define allocate(allocator, type) \
        (type*)(allocate_aligned((allocator), sizeof(type), alignof(type), __FILE__, __LINE__))

#   define allocate_size(allocator, size, cast_type) \
        (cast_type*)(allocate_aligned((allocator), (size), sizeof(void*), __FILE__, __LINE__))

#   define allocate_size_align(allocator, size, align, cast_type) \
        (cast_type*)(allocate_aligned((allocator), (size), (align), __FILE__, __LINE__))

#   define allocate_array(allocator, type, length) \
       (type*)(allocate_aligned((allocator), sizeof(type) * (length), alignof(type), __FILE__, __LINE__))

#   define reallocate_size(allocator, old_ptr, new_size, cast_type) \
        (cast_type*)(reallocate_aligned((allocator), (old_ptr), (new_size), __FILE__, __LINE__))

#   define reallocate_size_align(allocator, old_ptr, new_size, align, cast_type) \
        (cast_type*)(reallocate_aligned((allocator), (old_ptr), (new_size), (align), __FILE__, __LINE__))

#   define reallocate_array(allocator, type, old_ptr, new_cap) \
        (type*)(reallocate_aligned((allocator), (old_ptr), sizeof(type) * (new_cap), alignof(type), __FILE__, __LINE__))

#   define release(allocator, ptr) \
       allocate_free((allocator), (void*)(ptr), __FILE__, __LINE__)

#   define release_all(allocator) \
       allocate_free_all((allocator), __FILE__, __LINE__)


#else // NOVO_TRACE_ALLOC

#   define FN_ALLOCATE_UNALIGNED(f) void* f(Allocator* allocator, s64 size)
    typedef FN_ALLOCATE_UNALIGNED(FN_Allocate_Unaligned);
    NAPI FN_ALLOCATE_UNALIGNED(allocate_unaligned);

#   define FN_ALLOCATE_ALIGNED(f) void* f(Allocator* allocator, s64 size, s64 align)
    typedef FN_ALLOCATE_ALIGNED(FN_Allocate_Aligned);
    NAPI FN_ALLOCATE_ALIGNED(allocate_aligned);

#   define FN_REALLOCATE_UNALIGNED(f) void* f(Allocator* allocator, void* old_pointer, s64 old_size, s64 new_size)
    typedef FN_REALLOCATE_UNALIGNED(FN_Reallocate_Unaligned);
    NAPI FN_REALLOCATE_UNALIGNED(reallocate_unaligned);

    #define FN_REALLOCATE_ALIGNED(f) void* f(Allocator* allocator, void* old_pointer, s64 old_size, s64 new_size, s64 align)
    typedef FN_REALLOCATE_ALIGNED(FN_Reallocate_Analigned);
    NAPI FN_REALLOCATE_ALIGNED(reallocate_aligned);

#   define FN_FREE(f) void f(Allocator* allocator, void* ptr)
    typedef FN_FREE(FN_Free);
    NAPI FN_FREE(allocate_free);

#   define FN_FREE_ALL(f) void f(Allocator* allocator)
    typedef FN_FREE_ALL(FN_Free_All);
    NAPI FN_FREE_ALL(allocate_free_all);

#   define nallocate(allocator, type) \
       (type*)(allocate_aligned((allocator), sizeof(type), alignof(type)));

#   define nallocate_size(allocator, size, cast_type) \
        (cast_type*)(allocate_aligned((allocator), (size), alignof(void*)));

#   define nallocate_size_align(allocator, size, align, cast_type) \
        (cast_type*)(allocate_aligned((allocator), (size), (align)))

#   define nallocate_array(allocator, type, cap) \
       (type*)(allocate_aligned((allocator), sizeof(type) * (cap), alignof(type)))

#   define nreallocate_size(allocator, old_ptr, old_size,  new_size, cast_type) \
        (cast_type*)(reallocate_aligned((allocator), (old_ptr), (old_size), (new_size)))

#   define nreallocate_size_align(allocator, old_ptr, old_size, new_size, align, cast_type) \
        (cast_type*)(reallocate_aligned((allocator), (old_ptr), (old_size), (new_size), (align)))

#   define nreallocate_array(allocator, type, old_ptr, old_cap, new_cap) \
        (type*)(reallocate_aligned((allocator), (old_ptr), sizeof(type) * (old_cap), sizeof(type) * (new_cap), alignof(type)))

#   define nrelease(allocator, ptr) \
       allocate_free((allocator), (ptr))

#   define nrelease_all(allocator) \
       allocate_free_all((allocator))

#endif // NOVO_TRACE_ALLOC

}
