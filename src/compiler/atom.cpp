#include "atom.h"

namespace Novo {

void atom_table_create(Atom_Table *at, Allocator *allocator)
{
    atom_table_create(at, allocator, NOVO_ATOM_TABLE_DEFAULT_CAPACITY);
}

void atom_table_create(Atom_Table *at, Allocator *allocator, s64 capacity)
{
    at->allocator = allocator;

    at->capacity = capacity;

    at->hashes = allocate_array<u64>(allocator, capacity);
    at->ids = allocate_array<u32>(allocator, capacity);

    darray_create(allocator, &at->strings);
}

void atom_table_free(Atom_Table *at)
{
    free(at->allocator, at->hashes);
    free(at->allocator, at->ids);

    for (s64 i = 0; i < at->strings.count; i++) {
        free(at->allocator, at->strings[i].data);
    }

    darray_free(&at->strings);
}

static void atom_table_grow(Atom_Table *at)
{
    auto new_cap = at->capacity * 2;

    auto old_cap = at->capacity;
    auto old_hashes = at->hashes;
    auto old_ids = at->ids;

    auto new_hashes = allocate_array<u64>(at->allocator, new_cap);
    auto new_ids = allocate_array<u32>(at->allocator, new_cap);

    at->capacity = new_cap;
    at->hashes = new_hashes;
    at->ids = new_ids;

    for (s64 i = 0; i < old_cap; i++) {

        u64 hash = old_hashes[i];
        u64 hash_index = hash % at->capacity;

        u64 iteration_count = 0;

        while (iteration_count < at->capacity) {

            if (at->hashes[hash_index] == 0) {
                at->hashes[hash_index] = hash;
                at->ids[hash_index] = old_ids[i];
                break;
            } else {
                hash_index += 1;
                hash_index = hash_index >= at->capacity ? 0 : hash_index;
            }

            iteration_count += 1;
        }
    }

    free(at->allocator, old_hashes);
    free(at->allocator, old_ids);
}

Atom atom_get(Atom_Table *at, const String_Ref &str)
{
    u64 hash = hash_string(str.data, str.length);
    u32 hash_index = hash % at->capacity;

    u32 iteration_count = 0;

    while (iteration_count < at->capacity) {

        if (at->hashes[hash_index] == 0) {

            auto id64 = at->strings.count + 1;
            assert(id64 >= 0 && id64 < U32_MAX);
            u32 id = id64;

            at->hashes[hash_index] = hash;
            at->ids[hash_index] = id;

            darray_append(&at->strings, string_copy(at->allocator, str));

            return id;

        } else {

            if (at->hashes[hash_index] == hash) {
                u32 id = at->ids[hash_index];
                u32 index = id - 1;
                if (string_equal(at->strings[index], str)) {
                    return id;
                }
            }
        }

        hash_index += 1;
        if (hash_index >= at->capacity) hash_index = 0;
        iteration_count += 1;
    }

    atom_table_grow(at);
    return atom_get(at, str);
}

Atom atom_get(Atom_Table *at, const char *start, s64 length)
{
    return atom_get(at, String_Ref(start, length));
}

String &atom_string(Atom_Table *at, Atom atom)
{
    auto index = atom - 1;
    assert(index >= 0 && index < at->strings.count);
    return at->strings[index];
}

}
