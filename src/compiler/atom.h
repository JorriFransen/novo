#pragma once

#include <containers/darray.h>
#include <defines.h>
#include <nstring.h>

#define NOVO_ATOM_TABLE_DEFAULT_STRING_BLOCK_SIZE 2048

namespace Novo {

struct Allocator;

typedef u32 Atom;

struct Atom_String_Memory_Block
{
    char *mem;
    char *end;

    Atom_String_Memory_Block *next_block;
};

struct Atom_Table
{
    Allocator *allocator;

    u32 capacity;
    u32 next_block_size;

    u64 *hashes;
    u32 *ids;

    DArray<String> strings;

    Atom_String_Memory_Block *first_sting_block;
    Atom_String_Memory_Block *current_string_block;

};

NAPI extern Atom_Table g_atoms;
NAPI extern bool g_atoms_initialized;

NAPI void initialize_atoms(Allocator *allocator, s64 capacity);
NAPI void free_atoms();

NAPI Atom atom_get(const String_Ref &str);
NAPI Atom atom_get(const char *start, s64 length);

NAPI String &atom_string(Atom atom);

}
