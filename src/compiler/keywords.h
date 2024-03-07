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
    NOVO_KEYWORD(offsetof)  \
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

enum class Novo_Keyword {
    KW_INVALID,

#define NOVO_KEYWORD(kw) KW_##kw,
    ALL_NOVO_KEYWORDS
#undef NOVO_KEYWORD
};


struct KW_Info
{
    Novo_Keyword kind;
    Atom atom;
};

static KW_Info g_keyword_info[] = {
#define NOVO_KEYWORD(kw) { Novo_Keyword::KW_INVALID, 0 },
    ALL_NOVO_KEYWORDS
#undef NOVO_KEYWORD
};


// These are not 'reserved' names, but they are a lot like keywords
#define NOVO_EXTRA_ATOMS    \
    NOVO_EXTRA_ATOM(run)    \
    NOVO_EXTRA_ATOM(import) \
    NOVO_EXTRA_ATOM(insert) \

#define NOVO_EXTRA_ATOM(x) NAPI extern Atom g_atom_##x;
NOVO_EXTRA_ATOMS
#undef NOVO_EXTRA_ATOM

enum class Novo_Atom {
#define NOVO_ATOM(x) ATOM_##kw,
    NOVO_ATOMS
#undef NOVO_ATOM
};

NAPI void initialize_keywords();

}
