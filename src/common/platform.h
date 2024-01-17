#pragma once

#include "defines.h"
#include "nstring.h"

#if NPLATFORM_LINUX
#define NPLATFORM_PATH_SEPARATOR "/"
#elif NPLATFORM_WINDOWS
#define NPLATFORM_PATH_SEPARATOR "\\"
#else // NPLATFORM_LINUX
static_assert(false, "Unsupported platform!");
#endif // NPLATFORM_LINUX

namespace Novo {

struct Allocator;

NAPI String platform_dirname(Allocator *allocator, const String_Ref path);

}
