#pragma once

#include <defines.h>
#include <nstring.h>

#include "atom.h"
#include "source_pos.h"

namespace Novo {

struct Instance;

enum Token_Kind : u32
{
    TOK_INVALID = 0,

    // Ascii range (mostly here so the character literals can be used in switch statements)
    TOK_HASH   = '#', // 35
    TOK_LPAREN = '(', // 40
    TOK_RPAREN = ')', // 41
    TOK_STAR   = '*', // 42
    TOK_DOT    = '.', // 46
    TOK_LBRACK = '[', // 91
    TOK_RBRACK = ']', // 93

    TOK_INT = 256,
    TOK_REAL,
    TOK_STRING,
    TOK_CHAR,
    TOK_NAME,
    TOK_KEYWORD,

    TOK_RIGHT_ARROW,
    TOK_DOT_DOT,

    TOK_EQ,
    TOK_NEQ,
    TOK_LTEQ,
    TOK_GTEQ,

    TOK_ERROR,

    TOK_EOF,
    TOK_LAST = TOK_EOF,
};

struct Token
{
    Token_Kind kind;
    Atom atom;

    union
    {
        u64 integer;
        Real_Value real;
        char character;
    };

    u64 source_pos_id;
};

struct Lexer
{
    Instance* instance;

    String_Ref stream_name;

    const char* stream_start;
    const char* stream;
    const char* line_start;

    Token token;
    Source_Pos pos;
};

NAPI void lexer_create(Instance* instance, Lexer* out_lexer);
NAPI void lexer_init_stream(Lexer* lexer, const String_Ref stream, const String_Ref stream_name);
NAPI void lexer_destroy(Lexer* lexer);

NAPI bool next_token(Lexer* lexer);
NAPI bool is_token(Lexer* lexer, Token_Kind kind);
NAPI bool is_token(Lexer* lexer, char c);

NAPI String_Ref tmp_token_str(Token token);
NAPI String_Ref tmp_token_kind_str(Token_Kind kind);

}
