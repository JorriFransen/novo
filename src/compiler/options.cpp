#include "options.h"

namespace Novo {

Options default_options()
{
    Options result;
    result.input_file = nullptr;


#define NOVO_BOOL_OPTION(short_name, long_name, value, ...) result.long_name = value;
#define NOVO_STRING_OPTION(short_name, long_name, value, ...) result.long_name = value;
    ALL_NOVO_OPTIONS_X
#undef NOVO_BOOL_OPTION
#undef NOVO_STRING_OPTION


    return result;
}

}
