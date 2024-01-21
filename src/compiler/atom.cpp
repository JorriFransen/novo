#include "atom.h"

#include <memory/allocator.h>

#include <cassert>
#include <cstring>

namespace Novo {

Atom_Table g_atoms;
bool g_atoms_initialized = false;

void initialize_atoms(Allocator *allocator, s64 capacity)
{
    g_atoms.allocator = allocator;

    g_atoms.capacity = capacity;

    g_atoms.hashes = (u64 *)allocate(allocator, capacity * (sizeof(g_atoms.hashes[0]) + sizeof(g_atoms.ids[0])));
    g_atoms.ids = (u32 *)&g_atoms.hashes[capacity];

    darray_init(allocator, &g_atoms.strings);

    auto string_block_buffer_size = NOVO_ATOM_TABLE_DEFAULT_STRING_BLOCK_SIZE;
    g_atoms.next_block_size = string_block_buffer_size;
    auto string_block_size = sizeof(Atom_String_Memory_Block) + string_block_buffer_size;
    g_atoms.first_sting_block = (Atom_String_Memory_Block *)allocate(allocator, string_block_size);

    g_atoms.first_sting_block->mem = &((char *)g_atoms.first_sting_block)[sizeof(Atom_String_Memory_Block)];
    g_atoms.first_sting_block->end = g_atoms.first_sting_block->mem + string_block_buffer_size;
    g_atoms.first_sting_block->next_block = nullptr;

    g_atoms.current_string_block = g_atoms.first_sting_block;
}

void free_atoms()
{
    free(g_atoms.allocator, g_atoms.hashes);

    auto sb = g_atoms.first_sting_block;
    while (sb) {
        auto next = sb->next_block;

        free(g_atoms.allocator, sb);

        sb = next;
    }

    darray_free(&g_atoms.strings);
}

static void atom_table_grow()
{
    auto new_cap = g_atoms.capacity * 2;

    auto old_cap = g_atoms.capacity;
    auto old_hashes = g_atoms.hashes;
    auto old_ids = g_atoms.ids;

    auto new_hashes = (u64 *)allocate(g_atoms.allocator, new_cap * (sizeof(g_atoms.hashes[0]) + sizeof(g_atoms.ids[0])));
    auto new_ids = (u32 *)&new_hashes[new_cap];

    g_atoms.capacity = new_cap;
    g_atoms.hashes = new_hashes;
    g_atoms.ids = new_ids;

    for (s64 i = 0; i < old_cap; i++) {

        u64 hash = old_hashes[i];
        u64 hash_index = hash % g_atoms.capacity;

        u64 iteration_count = 0;

        while (iteration_count < g_atoms.capacity) {

            if (g_atoms.hashes[hash_index] == 0) {
                g_atoms.hashes[hash_index] = hash;
                g_atoms.ids[hash_index] = old_ids[i];
                break;
            } else {
                hash_index += 1;
                hash_index = hash_index >= g_atoms.capacity ? 0 : hash_index;
            }

            iteration_count += 1;
        }
    }

    free(g_atoms.allocator, old_hashes);
}

static char *atom_table_add_string(const String_Ref &str)
{
    auto sb = g_atoms.current_string_block;

    auto remaining = sb->end - sb->mem;
    auto required = str.length + 1;

    if (required > remaining) {

        // Try to find an existing block first
        Atom_String_Memory_Block *ex_block = nullptr;
        auto cb = g_atoms.first_sting_block;

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
            auto new_block_size = g_atoms.next_block_size;
            while (new_block_size < str.length * 2) new_block_size *= 2;
            g_atoms.next_block_size = new_block_size;

            auto total_size = new_block_size + sizeof(Atom_String_Memory_Block);
            auto new_block = (Atom_String_Memory_Block *)allocate(g_atoms.allocator, total_size);

            new_block->mem = &((char *)new_block)[sizeof(Atom_String_Memory_Block)];
            new_block->end = new_block->mem + new_block_size;
            new_block->next_block = nullptr;

            g_atoms.current_string_block->next_block = new_block;
            g_atoms.current_string_block = new_block;

            sb = new_block;
        }
    }

    memcpy(sb->mem, str.data, required);
    sb->mem[str.length] = '\0';

    char *result = sb->mem;
    sb->mem += required;
    return result;
}

Atom atom_get(const String_Ref &str)
{
    u64 hash = hash_string(str.data, str.length);
    u32 hash_index = hash % g_atoms.capacity;

    u32 iteration_count = 0;

    while (iteration_count < g_atoms.capacity) {

        if (g_atoms.hashes[hash_index] == 0) {

            auto id64 = g_atoms.strings.count + 1;
            assert(id64 >= 0 && id64 < U32_MAX);
            u32 id = id64;

            g_atoms.hashes[hash_index] = hash;
            g_atoms.ids[hash_index] = id;

            auto str_ptr = atom_table_add_string(str);
            darray_append(&g_atoms.strings, string(str_ptr, str.length));

            return id;

        } else {

            if (g_atoms.hashes[hash_index] == hash) {
                u32 id = g_atoms.ids[hash_index];
                u32 index = id - 1;
                if (string_equal(g_atoms.strings[index], str)) {
                    return id;
                }
            }
        }

        hash_index += 1;
        if (hash_index >= g_atoms.capacity) hash_index = 0;
        iteration_count += 1;
    }

    atom_table_grow();
    return atom_get(str);
}

Atom atom_get(const char *start, s64 length)
{
    return atom_get(String_Ref(start, length));
}

String &atom_string(Atom atom)
{
    auto index = atom - 1;
    assert(index >= 0 && index < g_atoms.strings.count);
    return g_atoms.strings[index];
}

}
