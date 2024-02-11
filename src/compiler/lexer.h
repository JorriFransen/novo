#pragma once

#include <defines.h>
#include <nstring.h>

#include "atom.h"

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

typedef u32 Token_Flags;
enum Token_Flag : Token_Flags {

    TOK_FLAG_NONE   = 0x00,
    TOK_FLAG_HEX    = 0x01,
    TOK_FLAG_BINARY = 0x02,
};

struct Token
{
    Token_Kind kind;
    Atom atom;

    Token_Flags flags;

    union
    {
        u64 integer;
        Real_Value real;
        char character;
    };


    u32 offset;
    u32 length;
};

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

NAPI String_Ref tmp_token_str(Token token);
NAPI String_Ref tmp_token_kind_str(Token_Kind kind);

}
