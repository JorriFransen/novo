#pragma once

#include "defines.h"
#include "allocator.h"

namespace Novo {

NAPI Allocator* c_allocator();

NAPI FN_ALLOCATOR(c_allocator_fn);

}

