
#include <memory/allocator.h>

#include "atom.h"

using namespace Novo;

static void single_match()
{
    auto allocator = c_allocator();

    Atom_Table at;
    atom_table_create(&at, allocator);

    auto cstr = "test string";
    Atom atom = atom_get(&at, cstr);
    Atom atom2 = atom_get(&at, cstr);

    assert(atom == atom2);

    assert(atom == atom_get(&at, "test string"));

    auto str1 = atom_string(&at, atom);
    auto str2 = atom_string(&at, atom2);

    assert(str1.data == str2.data);
    assert(string_equal(str1, str2));

    atom_table_free(&at);
}

static void multiple_match()
{
    auto allocator = c_allocator();

    Atom_Table at;
    atom_table_create(&at, allocator);

    Atom a1 = atom_get(&at, "a1");
    Atom a2 = atom_get(&at, "a2");
    Atom a3 = atom_get(&at, "a3");
    Atom a4 = atom_get(&at, "a4");
    Atom a5 = atom_get(&at, "a5");

    assert(a1 == atom_get(&at, "a1"));
    assert(a2 == atom_get(&at, "a2"));
    assert(a3 == atom_get(&at, "a3"));
    assert(a4 == atom_get(&at, "a4"));
    assert(a5 == atom_get(&at, "a5"));

    assert(a1 != a2);
    assert(a1 != a3);
    assert(a1 != a4);
    assert(a1 != a5);

    assert(a2 != a1);
    assert(a2 != a3);
    assert(a2 != a4);
    assert(a2 != a5);

    assert(a3 != a1);
    assert(a3 != a2);
    assert(a3 != a4);
    assert(a3 != a5);

    assert(a4 != a1);
    assert(a4 != a2);
    assert(a4 != a3);
    assert(a4 != a5);

    assert(a5 != a1);
    assert(a5 != a2);
    assert(a5 != a3);
    assert(a5 != a4);

    Atom a11 = atom_get(&at, "a11");
    assert(a1 != a11);

    Atom z1 = atom_get(&at, "z1");
    assert(a1 != z1);

    atom_table_free(&at);
}

static void growing()
{
    auto allocator = c_allocator();

    Atom_Table at;
    atom_table_create(&at, allocator, 2);

    assert(at.capacity == 2);
    Atom a1 = atom_get(&at, "a1");
    Atom a2 = atom_get(&at, "a2");
    assert(at.capacity == 2);
    Atom a3 = atom_get(&at, "a3");
    assert(at.capacity == 4);
    Atom a4 = atom_get(&at, "a4");
    assert(at.capacity == 4);
    Atom a5 = atom_get(&at, "a5");
    assert(at.capacity == 8);

    assert(a1 == atom_get(&at, "a1"));
    assert(a2 == atom_get(&at, "a2"));
    assert(a3 == atom_get(&at, "a3"));
    assert(a4 == atom_get(&at, "a4"));
    assert(a5 == atom_get(&at, "a5"));

    assert(a1 != a2);
    assert(a1 != a3);
    assert(a1 != a4);
    assert(a1 != a5);

    assert(a2 != a1);
    assert(a2 != a3);
    assert(a2 != a4);
    assert(a2 != a5);

    assert(a3 != a1);
    assert(a3 != a2);
    assert(a3 != a4);
    assert(a3 != a5);

    assert(a4 != a1);
    assert(a4 != a2);
    assert(a4 != a3);
    assert(a4 != a5);

    assert(a5 != a1);
    assert(a5 != a2);
    assert(a5 != a3);
    assert(a5 != a4);

    atom_table_free(&at);
}

int main(int argc, char *argv[]) {
    single_match();
    multiple_match();
    growing();
    return 0;
}
