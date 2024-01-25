#include "keywords.h"

#include "atom.h"

namespace Novo {

#define NOVO_KEYWORD(kw) \
    Atom g_atom_##kw; \
    Atom g_keyword_##kw;

ALL_NOVO_KEYWORDS

#undef NOVO_KEYWORD

Atom g_first_keyword_atom;
Atom g_last_keyword_atom;

static Atom g_kw_atoms[] = {
#define NOVO_KEYWORD(kw) 0,
    ALL_NOVO_KEYWORDS
#undef NOVO_KEYWORD
};

void initialize_keywords()
{
    int index = 0;

#define NOVO_KEYWORD(kw) \
    g_atom_##kw = atom_get(#kw); \
    g_keyword_##kw = g_atom_##kw; \
    g_kw_atoms[index++] = g_atom_##kw;

    ALL_NOVO_KEYWORDS
#undef NOVO_KEYWORD

    g_first_keyword_atom = g_kw_atoms[0];
    g_last_keyword_atom = g_kw_atoms[(sizeof(g_kw_atoms) / sizeof(g_kw_atoms[0])) - 1];
}

}
