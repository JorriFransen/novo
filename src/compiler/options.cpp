#include "options.h"

namespace Novo {

Options default_options()
{
    Options result;

    

#define NOVO_BOOL_OPTION(s, l, v) result.l = v;
#define NOVO_STRING_OPTION(s, l, v) result.l = v;
    ALL_NOVO_OPTIONS_X
#undef NOVO_BOOL_OPTION
#undef NOVO_STRING_OPTION


    return result;
}

}
