#pragma once

#include "defines.h"

namespace Novo {


enum class Allocator_Mode
{
    ALLOCATE,
    REALLOCATE,
    FREE,
    FREE_ALL,
};

typedef void *(*FN_Allocator)(Allocator_Mode mode, s64 size, s64 old_size, void *old_pointer, void *allocator_data, s64 options);

}
