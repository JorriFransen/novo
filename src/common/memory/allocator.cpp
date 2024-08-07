#include "allocator.h"

namespace Novo {

FN_ALLOCATE_UNALIGNED(allocate_unaligned)
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

FN_REALLOCATE_UNALIGNED(reallocate_unaligned)
{
#ifdef NOVO_TRACE_ALLOC
    auto ptr = allocator->fn(Allocator_Mode::REALLOCATE, new_size, 1, old_size, old_pointer, allocator->user_data, 0, file, line);
#else // NOVO_TRACE_ALLOC
    auto ptr = allocator->fn(Allocator_Mode::REALLOCATE, new_size, 1, old_size, old_pointer, allocator->user_data, 0);
#endif // NOVO_TRACE_ALLOC
    return ptr;
}

FN_REALLOCATE_ALIGNED(reallocate_aligned)
{
#ifdef NOVO_TRACE_ALLOC
    auto ptr = allocator->fn(Allocator_Mode::REALLOCATE, new_size, align, old_size, old_pointer, allocator->user_data, 0, file, line);
#else // NOVO_TRACE_ALLOC
    auto ptr = allocator->fn(Allocator_Mode::REALLOCATE, new_size, align, old_size, old_pointer, allocator->user_data, 0);
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
