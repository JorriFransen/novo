#pragma once

#include "defines.h"
#include "nstring.h"
#include <containers/darray.h>

#if NPLATFORM_LINUX
#define NPLATFORM_DEFAULT_EXE_EXTENSION ""
#define NPLATFORM_PATH_SEPARATOR "/"
#define NPLATFORM_DYNAMIC_LIB_PREFIX "lib"
#define NPLATFORM_STATIC_LIB_PREFIX "lib"
#define NPLATFORM_DYNAMIC_LIB_EXTENSION ".so"
#define NPLATFORM_STATIC_LIB_EXTENSION ".a"
#elif NPLATFORM_WINDOWS
#define NPLATFORM_DEFAULT_EXE_EXTENSION ".exe"
#define NPLATFORM_PATH_SEPARATOR "\\"
#define NPLATFORM_DYNAMIC_LIB_PREFIX ""
#define NPLATFORM_STATIC_LIB_PREFIX "lib"
#define NPLATFORM_DYNAMIC_LIB_EXTENSION ".dll"
#define NPLATFORM_STATIC_LIB_EXTENSION ".a"
#else // NPLATFORM_LINUX
static_assert(false, "Unsupported platform!");
#endif // NPLATFORM_LINUX

namespace Novo {

struct Allocator;

NAPI String platform_dirname(Allocator* allocator, String_Ref path);
NAPI String platform_filename(Allocator* allocator, String_Ref path);
NAPI String platform_exe_path(Allocator* allocator, const char* argv_0);

NAPI void platform_mkdir(const String_Ref path);

struct Command_Result
{
    s64 exit_code;
    bool success;

    String result_string;
    String error_string;
};

NAPI Command_Result platform_run_command(Array_Ref<String_Ref> command_line, Allocator* debug_allocator);
NAPI void platform_free_command_result(Command_Result *cres);

#if NPLATFORM_WINDOWS
String platform_windows_normalize_line_endings(Allocator* allocator, const String_Ref str);
#endif // NPLATFORM_WINDOWS

}
