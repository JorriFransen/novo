
#include <memory/allocator.h>
#include <nstring.h>

#include "atom.h"

#include <assert.h>

using namespace Novo;

static void single_match()
{
    auto allocator = c_allocator();

    initialize_atoms(allocator, 4);

    auto cstr = "test string";
    Atom atom = atom_get(cstr);
    Atom atom2 = atom_get(cstr);

    assert(atom == atom2);

    assert(atom == atom_get("test string"));

    auto str1 = atom_string(atom);
    auto str2 = atom_string(atom2);

    assert(str1.data == str2.data);
    assert(string_equal(str1, str2));

    free_atoms();
}

static void multiple_match()
{
    auto allocator = c_allocator();

    initialize_atoms(allocator, 8);

    Atom a1 = atom_get("a1");
    Atom a2 = atom_get("a2");
    Atom a3 = atom_get("a3");
    Atom a4 = atom_get("a4");
    Atom a5 = atom_get("a5");

    assert(a1 == atom_get("a1"));
    assert(a2 == atom_get("a2"));
    assert(a3 == atom_get("a3"));
    assert(a4 == atom_get("a4"));
    assert(a5 == atom_get("a5"));

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

    Atom a11 = atom_get("a11");
    assert(a1 != a11);

    Atom z1 = atom_get("z1");
    assert(a1 != z1);

    free_atoms();
}

static void growing()
{
    auto allocator = c_allocator();

    initialize_atoms(allocator, 2);

    assert(g_atoms.capacity == 2);
    Atom a1 = atom_get("a1");
    Atom a2 = atom_get("a2");
    assert(g_atoms.capacity == 2);
    Atom a3 = atom_get("a3");
    assert(g_atoms.capacity == 4);
    Atom a4 = atom_get("a4");
    assert(g_atoms.capacity == 4);
    Atom a5 = atom_get("a5");
    assert(g_atoms.capacity == 8);

    assert(a1 == atom_get("a1"));
    assert(a2 == atom_get("a2"));
    assert(a3 == atom_get("a3"));
    assert(a4 == atom_get("a4"));
    assert(a5 == atom_get("a5"));

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

    free_atoms();
}

int main(int argc, char* argv[]) {
    single_match();
    multiple_match();
    growing();
    return 0;
}
