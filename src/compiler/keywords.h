#pragma once

#include <defines.h>

#include "atom.h"

// IWYU pragma: no_include <assert.h>

namespace Novo {

#define ALL_NOVO_KEYWORDS   \
    NOVO_KEYWORD(if)        \
    NOVO_KEYWORD(else)      \
    NOVO_KEYWORD(return)    \
    NOVO_KEYWORD(while)     \
    NOVO_KEYWORD(for)       \
    NOVO_KEYWORD(switch)    \
    NOVO_KEYWORD(case)      \
    NOVO_KEYWORD(default)   \
    NOVO_KEYWORD(break)     \
    NOVO_KEYWORD(continue)  \
    NOVO_KEYWORD(sizeof)    \
    NOVO_KEYWORD(alignof)   \
    NOVO_KEYWORD(struct)    \
    NOVO_KEYWORD(union)     \
    NOVO_KEYWORD(enum)      \
    NOVO_KEYWORD(true)      \
    NOVO_KEYWORD(false)     \
    NOVO_KEYWORD(defer)     \
    NOVO_KEYWORD(cast)      \
    NOVO_KEYWORD(null)      \
    NOVO_KEYWORD(assert)


#define NOVO_KEYWORD(kw) \
    NAPI extern Atom g_atom_##kw; \
    NAPI extern Atom g_keyword_##kw;

ALL_NOVO_KEYWORDS

#undef NOVO_KEYWORD

NAPI extern Atom g_first_keyword_atom;
NAPI extern Atom g_last_keyword_atom;

NAPI void initialize_keywords();

}
