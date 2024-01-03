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


#define FN_ALLOCATOR(f) void * f(Allocator_Mode mode, s64 size, s64 align, s64 old_size, void *old_pointer, void *allocator_data, s64 options)
typedef FN_ALLOCATOR(FN_Allocator);
NAPI FN_ALLOCATOR(c_allocator);


#define FN_DEFAULT_ALLOCATE(f) void *f(s64 size)
typedef FN_DEFAULT_ALLOCATE(FN_Default_Allocate);
NAPI FN_DEFAULT_ALLOCATE(allocate);

#define FN_ALLOCATE(f) void *f(FN_Allocator allocator, s64 size)
typedef FN_ALLOCATE(FN_Allocate);
NAPI FN_ALLOCATE(allocate);

#define FN_DEFAULT_ALLOCATE_ALIGNED(f) void *f(s64 size, s64 align)
typedef FN_DEFAULT_ALLOCATE_ALIGNED(FN_Default_Allocate_Aligned);
NAPI FN_DEFAULT_ALLOCATE_ALIGNED(allocate_aligned);

#define FN_ALLOCATE_ALIGNED(f) void *f(FN_Allocator allocator, s64 size, s64 align)
typedef FN_ALLOCATE_ALIGNED(FN_Allocate_Aligned);
NAPI FN_ALLOCATE_ALIGNED(allocate_aligned);


#define FN_DEFAULT_FREE(f) void f(void *ptr)
typedef FN_DEFAULT_FREE(FN_Default_Free);
NAPI FN_DEFAULT_FREE(free);

#define FN_FREE(f) void f(FN_Allocator allocator, void *ptr)
typedef FN_FREE(FN_Free);
NAPI FN_FREE(free);

}
