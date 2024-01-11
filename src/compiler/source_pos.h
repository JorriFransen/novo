#pragma once

#include <defines.h>

namespace Novo {

struct Instance;

struct Source_Pos
{
    const char *name;
    u32 line;
    u32 offset;
    u32 length;
};

struct Source_Range
{
    u32 start_index;
    u32 end_index;
};

NAPI u32 source_range(Instance *instance, u32 pos_id_a, u32 pos_id_b);
NAPI u32 source_range(Instance *instance, u32 pos_id);
NAPI u32 source_range_start(Instance *instance, u32 range_id);
NAPI u32 source_range_end(Instance *instance, u32 range_id);

}
