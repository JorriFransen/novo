#pragma once

#include "defines.h"
#include "nstring.h"

namespace Novo {

NAPI void fs_chdir(const String_Ref path);
NAPI bool fs_is_realpath(const String_Ref path);
NAPI String fs_realpath(Allocator *allocator, const String_Ref path);
NAPI bool fs_is_directory(const String_Ref path);
NAPI bool fs_is_file(const String_Ref path);

}
