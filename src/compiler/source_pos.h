#pragma once

#include "nstring.h"

namespace Novo {

struct Source_Pos
{
    String_Ref name;
    u64 line;
    u64 index_in_line;
    u64 length;
};

}
