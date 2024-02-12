#pragma once

#include <defines.h>
#include <nstring.h>

#include "token.h"

namespace Novo {

struct Instance;

struct Lexer
{
    Instance* instance;

    String_Ref stream_name;
    s64 import_index;

    const char* stream_start;
    const char* stream;
    const char* line_start;

    Token token;
};

NAPI void lexer_create(Instance* instance, Lexer* out_lexer);
NAPI void lexer_init_stream(Lexer* lexer, const String_Ref stream, const String_Ref stream_name, s64 import_index);
NAPI void lexer_destroy(Lexer* lexer);

NAPI bool next_token(Lexer* lexer);
NAPI bool is_token(Lexer* lexer, Token_Kind kind);
NAPI bool is_token(Lexer* lexer, char c);

}
