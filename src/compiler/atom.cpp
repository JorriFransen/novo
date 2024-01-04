#include "atom.h"

namespace Novo {

void atom_table_create(Atom_Table *at, Allocator *allocator)
{
    atom_table_create(at, allocator, NOVO_DARRAY_DEFAULT_CAPACITY);
}

void atom_table_create(Atom_Table *at, Allocator *allocator, s64 capacity)
{
    at->allocator = allocator;
    darray_create(at->allocator, &at->strings, capacity);
}

void atom_table_free(Atom_Table *at)
{
    for (s64 i = 0; i < at->strings.count; i++) {
        free(at->allocator, at->strings[i].data);
    }

    darray_free(&at->strings);
}

Atom atom_get(Atom_Table *at, const String_Ref &str)
{
    for (s64 i = 0; i < at->strings.count; i++) {
        if (string_equal(at->strings[i], str)) {
            return i + 1;
        }
    }

    s64 next_index = at->strings.count + 1;
    assert(next_index >= 0 && next_index <= U32_MAX);

    darray_append(&at->strings, string_copy(at->allocator, str));
    return (Atom)next_index;
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
