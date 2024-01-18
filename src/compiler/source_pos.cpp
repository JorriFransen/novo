#include "source_pos.h"

#include <containers/darray.h>

#include "instance.h"

#include <assert.h>

namespace Novo {

u32 source_range(Instance *instance, u32 pos_id_a, u32 pos_id_b)
{
    Source_Range range = { pos_id_a, pos_id_b };
    u64 result = instance->source_ranges.count;
    darray_append(&instance->source_ranges, range);
    return result;
}

u32 source_range(Instance *instance, u32 pos_id)
{
    Source_Range range = { pos_id, pos_id };
    u64 result = instance->source_ranges.count;
    darray_append(&instance->source_ranges, range);
    return result;

}

u32 source_range_start(Instance *instance, u32 range_id)
{
    assert(range_id > 0 && range_id < instance->source_ranges.count);
    return instance->source_ranges[range_id].start_index;
}

u32 source_range_end(Instance *instance, u32 range_id)
{
    assert(range_id > 0 && range_id < instance->source_ranges.count);
    return instance->source_ranges[range_id].end_index;
}

}
