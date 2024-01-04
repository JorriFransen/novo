#pragma once

#include <containers/darray.h>
#include <defines.h>
#include <memory/allocator.h>
#include <nstring.h>

namespace Novo {

typedef u32 Atom;

struct Atom_Table
{
    Allocator *allocator;

    // TODO: Use some sort of hash table
    DArray<String> strings;
};

NAPI void atom_table_create(Atom_Table *at, Allocator *allocator);

NAPI Atom atom_get(Atom_Table *at, const String_Ref &str);
NAPI Atom atom_get(Atom_Table *at, const char *start, s64 length);

NAPI String &atom_string(Atom_Table *at, Atom atom);

}
