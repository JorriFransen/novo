#pragma once

#include <defines.h>

namespace Novo {

struct Source_Pos
{
    const char *name;
    u32 line;
    u32 start;
    u32 length;
};

}
