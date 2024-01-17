#pragma once

#include "defines.h"
#include "nstring.h"

namespace Novo {

struct Allocator;

NAPI String platform_dirname(Allocator *allocator, const String_Ref path);

}
