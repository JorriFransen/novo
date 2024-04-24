#pragma once

#include <defines.h>
#include <nstring.h>

#include "atom.h"

#include <assert.h>

namespace Novo {

enum class Novo_Keyword;

enum Token_Kind : u32
{
    TOK_INVALID = 0,

    // Ascii range (mostly here so the character literals can be used in switch statements)
    TOK_HASH   = '#', // 35
    TOK_LPAREN = '(', // 40
    TOK_RPAREN = ')', // 41
    TOK_STAR   = '*', // 42
    TOK_DOT    = '.', // 46
    TOK_LT     = '<', // 60
    TOK_GT     = '>', // 62
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

        Novo_Keyword keyword;
    };


    u32 offset;
    u32 length;
};

NINLINE const char* token_kind_to_string(Token_Kind kind) {
    switch (kind) {
        case TOK_INVALID: return "INVALID";
        case TOK_INT: return "INT";
        case TOK_REAL: return "REAL";
        case TOK_STRING: return "STRING";
        case TOK_CHAR: return "CHAR";
        case TOK_NAME: return "NAME";
        case TOK_KEYWORD: return "KEYWORD";
        case TOK_RIGHT_ARROW: return "->";
        case TOK_DOT_DOT: return "..";
        case TOK_EQ: return "==";
        case TOK_NEQ: return "!=";
        case TOK_LTEQ: return "<=";
        case TOK_GTEQ: return ">=";
        case TOK_EOF: return "EOF";

        default: assert(false);
    }

    assert(false);
    return nullptr;
}

NAPI bool is_binary_arithmetic_op(u32 op);
NAPI bool is_binary_cmp_op(u32 op);
NAPI bool is_binary_op(u32 op);

NAPI bool is_binary_arithmetic_op(Token_Kind op);
NAPI bool is_binary_cmp_op(Token_Kind op);
NAPI bool is_binary_op(Token_Kind op);

NAPI String_Ref tmp_token_str(Token token);
NAPI String_Ref tmp_token_kind_str(Token_Kind kind);

}
