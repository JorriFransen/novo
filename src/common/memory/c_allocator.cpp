#include "c_allocator.h"

#include "memory/freelist.h"
#include "memory/trace.h"

#include <stdlib.h>
#include <string.h>

namespace Novo {

bool g_c_allocator_initialized = false;
Allocator g_c_allocator;

#ifdef NOVO_TRACE_ALLOC
Allocator_Trace g_c_allocator_trace;
#endif // NOVO_TRACE_ALLOC


Allocator* c_allocator()
{
    return fl_allocator();
    // if (!g_c_allocator_initialized) {

    //     void* user_data = nullptr;

    //     #ifdef NOVO_TRACE_ALLOC
    //         init_allocator_trace(&g_c_allocator_trace);
    //         user_data = &g_c_allocator_trace;
    //     #endif // NOVO_TRACE_ALLOC

    //     g_c_allocator = { c_allocator_fn, user_data, ALLOCATOR_FLAG_NONE };
    //     g_c_allocator_initialized = true;

    // }

    // return &g_c_allocator;
}

FN_ALLOCATOR(c_allocator_fn)
{
    switch (mode) {

    case Allocator_Mode::ALLOCATE: {
        s64 actual_size = size + (align - 1) + sizeof(void*);

        trace_timer_start(malloc_time);
        u8* mem = (u8*)malloc(actual_size);
        trace_alloc_timer_end(allocator_data, malloc_time, mem);

        void **ptr = (void**)get_aligned((u64)mem + sizeof(void*), align);

        // Store the pointer returned by malloc
        ptr[-1] = mem;

        memset(ptr, 0, size);

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
        trace_release_timer_end(allocator_data, release_time, _old);

        break;
    }

    case Allocator_Mode::FREE_ALL:
        assert(false && "c_allocator does not support free all\n");
        break;
    }

    return nullptr;
}

}
