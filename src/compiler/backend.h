#pragma once

#include "defines.h"

namespace Novo {

struct Instance;

enum class Backend_Platform
{
    INVALID,
    LINUX,
    WINDOWS,
};

typedef bool (*Backend_Emit_FN)(Instance* inst);

struct Backend
{
    Backend_Platform target_platform;
    Backend_Emit_FN emit;
};

NAPI void backend_init(Backend* backend, Backend_Platform target_platform, Backend_Emit_FN emit_fn);
NAPI bool backend_emit(Backend* backend, Instance* inst);
NAPI s64 backend_integer_bit_size(Backend* backend);
NAPI Backend_Platform backend_default_platform();

}
