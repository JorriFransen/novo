#include "platform.h"

#include "memory/allocator.h"

namespace Novo
{

#ifdef NPLATFORM_LINUX

#include <libgen.h>

String platform_dirname(Allocator *allocator, const String_Ref path)
{
    auto path_copy = string_copy(allocator, path);
    NSTRING_ASSERT_ZERO_TERMINATION(path_copy);

    auto result = dirname(path_copy.data);

    auto result_copy = string_copy(allocator, result);

    if (!(allocator->flags & ALLOCATOR_FLAG_CANT_FREE)) {
        free(allocator, path_copy.data);
    }

    return result_copy;
}

#else // NPLATFORM_LINUX
static_assert(false, "Unsupported platform")
#endif // NPLATFORM_LINUX

}
