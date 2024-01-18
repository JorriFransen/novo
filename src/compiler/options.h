#pragma once

#include <defines.h>

namespace Novo {

#define ALL_NOVO_OPTIONS_X                                                  \
    NOVO_BOOL_OPTION('v', verbose, false, "Enable verbose output", "VALUE") \
    NOVO_BOOL_OPTION('p', print_ast, false, "Print ast", "VALUE") \
    NOVO_STRING_OPTION('o', output, "", "Output file name", "FILENAME")     \


struct Options  {

    const char *input_file;

#define NOVO_BOOL_OPTION(short_name, long_name, value, ...) bool long_name = value;
#define NOVO_STRING_OPTION(short_name, long_name, value, ...) const char *long_name = value;
ALL_NOVO_OPTIONS_X
#undef NOVO_BOOL_OPTION
#undef NOVO_STRING_OPTION

};

NAPI Options default_options();

}
