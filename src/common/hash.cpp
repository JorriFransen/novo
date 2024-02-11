#include "hash.h"

#include "nstring.h"

#include <cassert>
#include <cstring>

namespace Novo {

u64 hash_key(s64 key)
{
    if (key == 0) key++;

    key = (~key) + (key << 21); // key = (key << 21) - key - 1;
    key = key ^ (key >> 24);
    key = (key + (key << 3)) + (key << 8); // key * 265
    key = key ^ (key >> 14);
    key = (key + (key << 2)) + (key << 4); // key * 21
    key = key ^ (key >> 28);
    key = key + (key << 31);

    assert(key);

    return key;
}

u64 hash_key(const char *str)
{
    return hash_string(str, strlen(str));
}

NAPI u64 hash_combine(u64 l, u64 r)
{
    return l ^ (r + 0x517cc1b727220a95 + (l << 6) + (l >> 2));
}

}

