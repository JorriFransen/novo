#include "platform.h"
#include <cassert>

#ifdef ZPLATFORM_LINUX

#include <stdlib.h> // IWYU pragma: keep
#include <linux/limits.h>
#include <sys/stat.h>
#include <unistd.h>

namespace Novo {

void fs_chdir(const String_Ref path)
{
    NSTRING_ASSERT_ZERO_TERMINATION(path);

    int res = chdir(path.data);
    assert(res == 0);
}

bool fs_is_realpath(const String_Ref path)
{
    NSTRING_ASSERT_ZERO_TERMINATION(path);

    if (path.length <= 0) return false;

    if (path[0] == '/') {
        struct stat sb;
        return stat(path.data, &sb) == 0;
    }

    return false;
}

String fs_realpath(Allocator *allocator, const String_Ref path)
{
    NSTRING_ASSERT_ZERO_TERMINATION(path);

    char buf[PATH_MAX] ;

    auto result = realpath(path.data, buf);
    assert(result == buf && "realpath failed, input path might be invalid!");

    return string_copy(allocator, buf);
}

bool fs_is_directory(const String_Ref path)
{
    NSTRING_ASSERT_ZERO_TERMINATION(path);

    struct stat sb;
    if (stat(path.data, &sb) == 0) {
        return S_ISDIR(sb.st_mode);
    }

    return false;
}

bool fs_is_file(const String_Ref path)
{
    NSTRING_ASSERT_ZERO_TERMINATION(path);

    struct stat sb;
    if (stat(path.data, &sb) == 0) {
        return S_ISREG(sb.st_mode);
    }

    return false;
}

#endif // ZPLATFORM_LINUX
}
