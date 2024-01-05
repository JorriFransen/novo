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

    at->hashes = (u64 *)allocate(allocator, capacity * (sizeof(at->hashes[0]) + sizeof(at->ids[0])));
    at->ids = (u32 *)&at->hashes[capacity];

    darray_create(allocator, &at->strings);

    auto string_block_buffer_size = NOVO_ATOM_TABLE_DEFAULT_STRING_BLOCK_SIZE;
    at->next_block_size = string_block_buffer_size;
    auto string_block_size = sizeof(Atom_String_Memory_Block) + string_block_buffer_size;
    at->first_sting_block = (Atom_String_Memory_Block *)allocate(allocator, string_block_size);

    at->first_sting_block->mem = &((char *)at->first_sting_block)[sizeof(Atom_String_Memory_Block)];
    at->first_sting_block->end = at->first_sting_block->mem + string_block_buffer_size;
    at->first_sting_block->next_block = nullptr;

    at->current_string_block = at->first_sting_block;
}

void atom_table_free(Atom_Table *at)
{
    free(at->allocator, at->hashes);

    auto sb = at->first_sting_block;
    while (sb) {
        auto next = sb->next_block;

        free(at->allocator, sb);

        sb = next;
    }

    darray_free(&at->strings);
}

static void atom_table_grow(Atom_Table *at)
{
    auto new_cap = at->capacity * 2;

    auto old_cap = at->capacity;
    auto old_hashes = at->hashes;
    auto old_ids = at->ids;

    auto new_hashes = (u64 *)allocate(at->allocator, new_cap * (sizeof(at->hashes[0]) + sizeof(at->ids[0])));
    auto new_ids = (u32 *)&new_hashes[new_cap];

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
}

static char *atom_table_add_string(Atom_Table *at, const String_Ref &str)
{
    auto sb = at->current_string_block;

    auto remaining = sb->end - sb->mem;
    auto required = str.length + 1;

    if (required > remaining) {

        // Try to find an existing block first
        Atom_String_Memory_Block *ex_block = nullptr;
        auto cb = at->first_sting_block;

        // sb should be the last block in the chain
        while (cb != sb) {
            auto next = cb->next_block;

            auto r = cb->end - sb->end;

            if (required <= r) {
                ex_block = cb;
                break;
            }

            cb = next;
        }

        if (ex_block) {
            sb = ex_block;
        } else {
            auto new_block_size = at->next_block_size;
            while (new_block_size < str.length * 2) new_block_size *= 2;
            at->next_block_size = new_block_size;

            auto total_size = new_block_size + sizeof(Atom_String_Memory_Block);
            auto new_block = (Atom_String_Memory_Block *)allocate(at->allocator, total_size);

            new_block->mem = &((char *)new_block)[sizeof(Atom_String_Memory_Block)];
            new_block->end = new_block->mem + new_block_size;
            new_block->next_block = nullptr;

            at->current_string_block->next_block = new_block;
            at->current_string_block = new_block;

            sb = new_block;
        }
    }

    memcpy(sb->mem, str.data, required);
    sb->mem[str.length] = '\0';

    char *result = sb->mem;
    sb->mem += required;
    return result;
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

            auto str_ptr = atom_table_add_string(at, str);
            darray_append(&at->strings, string(str_ptr, str.length));

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
