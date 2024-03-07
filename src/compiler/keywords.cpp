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

#define NOVO_EXTRA_ATOM(x) Atom g_atom_##x;
NOVO_EXTRA_ATOMS
#undef NOVO_EXTRA_ATOM

void initialize_keywords()
{
    int index = 0;

#define NOVO_KEYWORD(kw) \
    g_atom_##kw = atom_get(#kw); \
    g_keyword_##kw = g_atom_##kw; \
    g_keyword_info[index++] = { Novo_Keyword::KW_##kw, g_atom_##kw }; \

    ALL_NOVO_KEYWORDS
#undef NOVO_KEYWORD

    g_first_keyword_atom = g_keyword_info[0].atom;
    g_last_keyword_atom = g_keyword_info[(sizeof(g_keyword_info) / sizeof(g_keyword_info[0])) - 1].atom;



#define NOVO_EXTRA_ATOM(x) g_atom_##x = atom_get(#x);
    NOVO_EXTRA_ATOMS
#undef NOVO_EXTRA_ATOM
}

}
