#pragma once

#include <defines.h>

namespace Novo {

#define ALL_NOVO_OPTIONS_X                \
    NOVO_BOOL_OPTION('v', verbose, false) \
    NOVO_STRING_OPTION('o', output, "")   \
    NOVO_STRING_OPTION('a', another, "some_other")   \


struct Options {

#define NOVO_BOOL_OPTION(s, l, v) bool l = v;
#define NOVO_STRING_OPTION(s, l, v) const char *l = v;
ALL_NOVO_OPTIONS_X
#undef NOVO_BOOL_OPTION
#undef NOVO_STRING_OPTION

};

NAPI Options default_options();

}
