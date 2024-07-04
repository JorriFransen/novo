#include "backend.h"

#include <memory/arena.h>
#include <platform.h>

#include "instance.h"

namespace Novo {

void backend_init(Backend* backend, Backend_Platform target_platform, Backend_Emit_FN emit_fn)
{
    backend->target_platform = target_platform;
    backend->emit = emit_fn;
}

bool backend_emit(Backend* backend, Instance* inst) {
    return backend->emit(inst);
}

s64 backend_integer_bit_size(Backend* backend)
{
    switch (backend->target_platform) {
        case Backend_Platform::INVALID: assert(false); break;

        case Backend_Platform::LINUX: {
            Temp_Arena ta = temp_arena(nullptr);
            Command_Result cr = platform_run_command(Array_Ref<String_Ref>({"getconf", "WORD_BIT"}), ta.arena);

            assert(cr.success);

            s64 result = convert_string_to_signed(cr.result_string);
            assert(result >= 32);

            temp_arena_release(ta);

            return result;
            break;
        }

        case Backend_Platform::WINDOWS: {
            // Always 32 on 64 bit windows
            return 32;
        }
    }

    assert(false);
    return -1;
}

Backend_Platform backend_default_platform()
{
    #ifdef NPLATFORM_LINUX
        return Backend_Platform::LINUX;
    #elif NPLATFORM_WINDOWS
        return Backend_Platform::WINDOWS;
    #else // NPLATFORM_WINDOWS
        STATIC_ASSERT(false, "Unsupported platform!");
    #endif // NPLATFORM_LINUX
}

}
