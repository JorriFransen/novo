#pragma once

#include "defines.h"

namespace Novo {

NAPI u64 hash_key(s64 key);
NAPI u64 hash_key(const char *str);

template <typename T>
NAPI u64 hash_key(T *ptr) {
    return hash_key((u64)ptr);
}

NAPI u64 hash_combine(u64 l, u64 r);

}

