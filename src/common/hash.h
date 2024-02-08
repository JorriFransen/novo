#pragma once

#include "defines.h"

namespace Novo {

NAPI u64 hash_key(s64 key);
NAPI u64 hash_key(const char *str);

template <typename T>
s64 hash_key(T *ptr) {
    return hash_key((s64)ptr);
}


}

