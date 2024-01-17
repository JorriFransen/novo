#include "platform.h"

#include "memory/allocator.h"


#if NPLATFORM_LINUX
#include <libgen.h>
#elif NPLATFORM_WINDOWS
#include <cstring>
#include <stdlib.h>
#else // NPLATFORM_LINUX
static_assert(false, "Unsupported platform")
#endif // NPLATFORM_LINUX

namespace Novo
{

#if NPLATFORM_LINUX

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

#elif NPLATFORM_WINDOWS

String platform_dirname(Allocator *allocator, const String_Ref path)
{
    char drive[_MAX_DRIVE];
    char dir[_MAX_DIR];

    errno_t err = _splitpath_s(path.data, drive, _MAX_DRIVE, dir, _MAX_DIR, nullptr, 0, nullptr, 0);
    assert(err == 0);

    auto drive_length = strlen(drive);
    auto dir_length = strlen(dir);
    s64 result_length = drive_length + dir_length;

    String result = {
        .data = allocate_array<char>(allocator, result_length + 1),
        .length = result_length,
    };

    memcpy(result.data, drive, drive_length);
    memcpy(result.data + drive_length, dir, dir_length);
    result.data[result_length] = '\0';

    return result;
}

#else // NPLATFORM_LINUX
static_assert(false, "Unsupported platform")
#endif // NPLATFORM_LINUX

}
