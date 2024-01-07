#pragma once

#include <defines.h>

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

#define FN_ALLOCATOR(f) void * f(Allocator_Mode mode, s64 size, u64 align, s64 old_size, void *old_pointer, void *allocator_data, s64 options)
typedef FN_ALLOCATOR(FN_Allocator);

struct Allocator {
    FN_Allocator *fn;
    void *user_data;

    Allocator_Flags flags;
};

NAPI Allocator *c_allocator();

NAPI FN_ALLOCATOR(c_allocator_fn);

#define FN_DEFAULT_ALLOCATE(f) void *f(s64 size)
typedef FN_DEFAULT_ALLOCATE(FN_Default_Allocate);
NAPI FN_DEFAULT_ALLOCATE(allocate);

#define FN_ALLOCATE(f) void *f(Allocator *allocator, s64 size)
typedef FN_ALLOCATE(FN_Allocate);
NAPI FN_ALLOCATE(allocate);

#define FN_DEFAULT_ALLOCATE_ALIGNED(f) void *f(s64 size, u64 align)
typedef FN_DEFAULT_ALLOCATE_ALIGNED(FN_Default_Allocate_Aligned);
NAPI FN_DEFAULT_ALLOCATE_ALIGNED(allocate_aligned);

#define FN_ALLOCATE_ALIGNED(f) void *f(Allocator *allocator, s64 size, u64 align)
typedef FN_ALLOCATE_ALIGNED(FN_Allocate_Aligned);
NAPI FN_ALLOCATE_ALIGNED(allocate_aligned);


#define FN_DEFAULT_FREE(f) void f(void *ptr)
typedef FN_DEFAULT_FREE(FN_Default_Free);
NAPI FN_DEFAULT_FREE(free);

#define FN_FREE(f) void f(Allocator *allocator, void *ptr)
typedef FN_FREE(FN_Free);
NAPI FN_FREE(free);

#define FN_FREE_ALL(f) void f(Allocator *allocator)
typedef FN_FREE_ALL(FN_Free_All);
NAPI FN_FREE_ALL(free_all);

template <typename ET>
ET *allocate_array(Allocator *allocator, s64 size) {
    return (ET *)allocate(allocator, size * sizeof(ET));
}

template <typename T>
T *allocate(Allocator *allocator) {
    return (T *)allocate(allocator, sizeof(T));
}

}
