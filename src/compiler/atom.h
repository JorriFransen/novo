#pragma once

#include <containers/darray.h>
#include <defines.h>
#include <memory/allocator.h>
#include <nstring.h>

#define NOVO_ATOM_TABLE_DEFAULT_CAPACITY 32

namespace Novo {

typedef u32 Atom;

struct Atom_Table
{
    Allocator *allocator;

    u32 capacity;

    u64 *hashes;
    u32 *ids;

    DArray<String> strings;
};

NAPI void atom_table_create(Atom_Table *at, Allocator *allocator);
NAPI void atom_table_create(Atom_Table *at, Allocator *allocator, s64 capacity);
NAPI void atom_table_free(Atom_Table *at);

NAPI Atom atom_get(Atom_Table *at, const String_Ref &str);
NAPI Atom atom_get(Atom_Table *at, const char *start, s64 length);

NAPI String &atom_string(Atom_Table *at, Atom atom);

}
