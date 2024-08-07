#pragma once

#include <defines.h>
#include <nstring.h>

namespace Novo {

#define ALL_NOVO_OPTIONS_X                                                                  \
    NOVO_BOOL_OPTION('v', verbose, false, "Enable debug output", "[VALUE]")                 \
    NOVO_BOOL_OPTION('t', trace, false, "Enable trace output", "[VALUE]")                   \
    NOVO_BOOL_OPTION('p', print_ast, false, "Print ast", "[VALUE]")                         \
    NOVO_BOOL_OPTION('b', print_bytecode, false, "Print bytecode", "[VALUE]")               \
    NOVO_STRING_OPTION('o', output, nullptr, "Output file name", "FILENAME")                \
    NOVO_BOOL_OPTION('k', keep_c_backend_output, false, "Keep c backend output", "[VALUE]") \
    NOVO_BOOL_OPTION('n', no_backend, false, "Disable (c) backend", "[VALUE]")              \


struct Options  {

    const char* argv_0;
    const char* input_file;

#define NOVO_BOOL_OPTION(short_name, long_name, value, ...) bool long_name = value;
#define NOVO_STRING_OPTION(short_name, long_name, value, ...) const char* long_name = value;
ALL_NOVO_OPTIONS_X
#undef NOVO_BOOL_OPTION
#undef NOVO_STRING_OPTION

    String_Ref exe_dir;

};

NAPI Options default_options();

}
