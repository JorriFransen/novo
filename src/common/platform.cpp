#include "platform.h"

#include "memory/allocator.h"

#include <assert.h>

#if NPLATFORM_LINUX

#include <limits.h> // IWYU pragma: keep
#include <libgen.h>
#include <unistd.h>

#elif NPLATFORM_WINDOWS
#include <windows.h>
#include <cstring>
#include <stdlib.h>
#else // NPLATFORM_LINUX
static_assert(false, "Unsupported platform")
#endif // NPLATFORM_LINUX

namespace Novo
{

#if NPLATFORM_LINUX

String platform_dirname(Allocator* allocator, const String_Ref path)
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

String platform_exe_path(Allocator* allocator, const char* argv_0)
{
    char exe_path[PATH_MAX];
    ssize_t exe_path_length = readlink("/proc/self/exe", exe_path, PATH_MAX);
    assert(exe_path_length != -1);

    return string_copy(allocator, exe_path, exe_path_length);
}


#elif NPLATFORM_WINDOWS

String platform_dirname(Allocator* allocator, String_Ref path)
{
    s64 last_index = path.length;

    for (s64 i = path.length - 1; i >= 0; i--) {
        if (path[i] == '/' || path[i] == '\\') {
            last_index = i;
            break;
        }
    }

    if (last_index == 0) {
        return {};
    } else if (last_index < path.length) {
        return string_copy(allocator, path.data, last_index);
    }

    assert(last_index == path.length);
    return string_copy(allocator, path);
}

String platform_exe_path(Allocator* allocator, const char* argv_0)
{
    const size_t buf_length = 2048;
    char buf[buf_length];

    DWORD result = GetModuleFileNameA(nullptr, buf, buf_length);

    if (result >= 0 && result < buf_length) {
        return string_copy(allocator, buf, result);
    } else {
        DWORD err = GetLastError();
        if (err == ERROR_INSUFFICIENT_BUFFER) {
            assert(false && "[platform_exe_path] ERROR_INSUFFICIENT_BUFFER");
        } else {

            assert(false && "[platform_exe_path] Unknown error");
        }
    }

    assert(false);
    return {};
}

#else // NPLATFORM_LINUX
static_assert(false, "Unsupported platform")
#endif // NPLATFORM_LINUX

}
