#include "lexer.h"

#include <containers/darray.h>

#include "atom.h"
#include "instance.h"
#include "keywords.h"
#include "source_pos.h"

#include <cassert>
#include <cctype>
#include <cerrno>
#include <cfloat>
#include <cmath>
#include <cstdlib>

namespace Novo {

static bool lex_int(Lexer* lexer);
static bool lex_real(Lexer* lexer);

void lexer_create(Instance* instance, Lexer* out_lexer)
{
    out_lexer->instance = instance;
    out_lexer->stream_start = nullptr;
    out_lexer->stream = nullptr;
    out_lexer->line_start = nullptr;

    out_lexer->token = {};
}

void lexer_init_stream(Lexer* lexer, const String_Ref stream, const String_Ref stream_name, s64 import_index, u32 offset)
{
    lexer->stream_name = stream_name;
    lexer->import_index = import_index;
    lexer->offset = offset;

    DArray<u32>* newline_offsets = &lexer->instance->imported_files[import_index].newline_offsets;
    if (!newline_offsets->data) {
        darray_init(&lexer->instance->ast_allocator, newline_offsets);
    }

    lexer->stream_start = stream.data;
    lexer->stream = stream.data;
    lexer->line_start = stream.data;

    lexer->token.kind = TOK_INVALID;

    NSTRING_ASSERT_ZERO_TERMINATION(stream_name);

    next_token(lexer);
}

void lexer_destroy(Lexer* lexer)
{
    assert(lexer && lexer->stream_start && lexer->stream);

    *lexer = {};
}

bool next_token(Lexer* lex)
{
    if (lex->token.kind == TOK_ERROR) {
        return false;
    }

    DArray<u32> *newline_offsets = &lex->instance->imported_files[lex->import_index].newline_offsets;

    assert(lex && lex->stream_start && lex->stream);

next_token__start_lexing_token:
    lex->token.kind = TOK_INVALID;
    lex->token.offset = (lex->stream - lex->stream_start) + lex->offset;
    lex->token.flags = TOK_FLAG_NONE;
    auto start = lex->stream;

    switch (*lex->stream) {

#define TWO_CHAR_TOKEN_CASE(first_char, second_char, two_char_kind) \
case (first_char): {                                                \
    lex->token.kind = (Token_Kind)*lex->stream;                     \
    lex->stream += 1;                                               \
    if (*lex->stream == (second_char)) {                            \
        lex->token.kind = (two_char_kind);                          \
        lex->stream += 1;                                           \
    }                                                               \
    break;                                                          \
}


        case 0: {
            lex->token.kind = TOK_EOF;
            break;
        }

        case ' ': case '\n': case '\r': case '\t': {
            while (isspace(*lex->stream)) {
                if (*lex->stream == '\n') {
                    lex->line_start = lex->stream + 1;
                    darray_append(newline_offsets, (u32)(lex->stream - lex->stream_start) + lex->offset);
                }
                lex->stream += 1;
            }

            goto next_token__start_lexing_token;
            break;
        }

        case '"': {
            lex->token.kind = TOK_STRING;
            lex->stream += 1;

            while (true) {

                if (*lex->stream == '\\') {
                    lex->stream += 2;
                } else if (*lex->stream == '"') {
                    break;
                } else {
                    lex->stream += 1;
                }
            }

            lex->stream += 1;
            break;
        }

        case '\'': {
            lex->token.kind = TOK_CHAR;
            lex->stream += 1;
            lex->token.character = *lex->stream;
            lex->stream += 1;

            if (lex->token.character == '\\') {

                s64 special_index = is_escape_character(*lex->stream);
                if (special_index < 0) {
                    instance_fatal_error(lex->instance, source_pos(lex), "Invalid escape character: '\\%c'", *lex->stream);
                    return false;
                }

                lex->stream += 1;

                lex->token.character = get_special_char(special_index);
            }

            if (*lex->stream != '\'') {

                instance_fatal_error(lex->instance, source_pos(lex), "Exected \"'\" to end character literal");
                return false;
            }

            lex->stream += 1;
            break;
        }

        case '/': {
            lex->token.kind = (Token_Kind)'/';
            lex->stream += 1;
            if (*lex->stream == '/') {
                // Single line comment
                while (*lex->stream && *lex->stream != '\n') {
                    lex->stream += 1;
                }
                goto next_token__start_lexing_token;
            }
            break;
        }

        case '.': {
            if (isdigit(lex->stream[1])) {
                if (!lex_real(lex)) return false;
            } else if (lex->stream[1] == '.') {
                lex->token.kind = TOK_DOT_DOT;
                lex->stream += 2;
            } else {
                lex->token.kind = (Token_Kind)*lex->stream;
                lex->stream += 1;
            }
            break;
        }

        case '0': case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8': case '9':
        {
            while (isdigit(*lex->stream)) {
                lex->stream += 1;
            }

            char c = lex->stream[0];
            char cc = lex->stream[1];
            lex->stream = start;

            if ((c == '.' && cc != '.') || tolower(c) == 'e') {
                if (!lex_real(lex)) return false;
            } else {
                if (!lex_int(lex)) return false;
            }
            break;
        }

        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h': case 'i': case 'j': case 'k': case 'l': case 'm':
        case 'n': case 'o': case 'p': case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x': case 'y': case 'z':
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M':
        case 'N': case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
        case '_':
        {
            lex->token.kind = TOK_NAME;
            while (isalnum(*lex->stream) || *lex->stream == '_') {
                lex->stream += 1;
            }
            break;
        }

        TWO_CHAR_TOKEN_CASE('=', '=', TOK_EQ);
        TWO_CHAR_TOKEN_CASE('!', '=', TOK_NEQ);
        TWO_CHAR_TOKEN_CASE('<', '=', TOK_LTEQ);
        TWO_CHAR_TOKEN_CASE('>', '=', TOK_GTEQ);
        TWO_CHAR_TOKEN_CASE('-', '>', TOK_RIGHT_ARROW);

        default: {
            if (*lex->stream && std::isprint(*lex->stream)) {
                lex->token.kind = (Token_Kind)*lex->stream;
                lex->stream += 1;
            } else {
                instance_fatal_error(lex->instance, source_pos(lex), "Unexpected character: '%c', value: '%d'", *lex->stream, *lex->stream);
                lex->token.kind = TOK_ERROR;
                return false;
            }
            break;
        }

#undef TWO_CHAR_TOKEN_CASE

    }

    auto length = lex->stream - start;
    lex->token.length = length;

    if (length) {

            if (lex->token.kind == TOK_STRING) {
                const char* err_char = nullptr;
                String str_lit = convert_escape_characters_to_special_characters(&lex->instance->temp_allocator, String_Ref(start, length), &err_char);
                if (err_char) {
                    instance_fatal_error(lex->instance, source_pos(lex), "Invalid escape sequence in string literal: '\\%c'", *err_char);
                    lex->token.kind = TOK_ERROR;
                    return false;
                }

                lex->token.atom = atom_get(str_lit);

            } else {

                lex->token.atom = atom_get(start, length);
            }

        if (lex->token.kind == TOK_NAME && (lex->token.atom >= g_first_keyword_atom && lex->token.atom <= g_last_keyword_atom)) {
            lex->token.kind = TOK_KEYWORD;

            bool found = false;
            for (s64 i = 0; i < sizeof(g_keyword_info) / sizeof(g_keyword_info[0]); i++) {
                if (lex->token.atom == g_keyword_info[i].atom) {
                    found = true;
                    lex->token.keyword = g_keyword_info[i].kind;
                    break;
                }
            }
            assert(found);
        }

    } else {
        lex->token.atom = {};
    }

    return true;
}

bool is_token(Lexer* lexer, Token_Kind kind)
{
    return lexer->token.kind == kind;
}

bool is_token(Lexer* lexer, char c)
{
    return is_token(lexer, (Token_Kind)c);
}

NINLINE u8 char_to_digit(int i) {
    switch (i) {
        case '0': return 0;
        case '1': return 1;
        case '2': return 2;
        case '3': return 3;
        case '4': return 4;
        case '5': return 5;
        case '6': return 6;
        case '7': return 7;
        case '8': return 8;
        case '9': return 9;
        case 'a': case 'A': return 10;
        case 'b': case 'B': return 11;
        case 'c': case 'C': return 12;
        case 'd': case 'D': return 13;
        case 'e': case 'E': return 14;
        case 'f': case 'F': return 15;

        default: return 0;
    }
}

static bool lex_int(Lexer* lexer)
{
    assert(lexer);

    u64 base = 10;
    u64 result = 0;

    auto start = lexer->stream;
    u64 length = 0;

    Token_Flags flags = TOK_FLAG_NONE;

    if (*lexer->stream == '0') {
        lexer->stream += 1;

        if (tolower(*lexer->stream) == 'x') {
            lexer->stream += 1;
            base = 16;
            start = lexer->stream;
            flags |= TOK_FLAG_HEX;
        } else if (tolower(*lexer->stream) == 'b') {
            lexer->stream += 1;
            base = 2;
            start = lexer->stream;
            flags |= TOK_FLAG_BINARY;
        }
    }

    while (true) {

        u64 digit = char_to_digit((int)*lexer->stream);
        if (digit == 0 && *lexer->stream != '0') {
            break;
        }

        if (digit >= base) {
            assert(false); // Ensure position is reported correctly
            instance_fatal_error(lexer->instance, source_pos(lexer), "Digit '%c' out of range for base %llu", *lexer->stream, base);
            lexer->token.kind = TOK_ERROR;
            return false;
        }

        if (result > (U64_MAX - digit) / base) {

            assert(false); // Ensure position is reported correctly
            instance_fatal_error(lexer->instance, source_pos(lexer), "Integer literal overflow: '%.*s'", (int)length + 1, start);
            lexer->token.kind = TOK_ERROR;

            // Skip the rest of this integer
            while (isdigit(*lexer->stream)) lexer->stream += 1;
            return false;
        }

        result = result * base + digit;
        lexer->stream += 1;
        length += 1;
    }

    if (lexer->stream == start) {

        assert(false); // Ensure position is reported correctly
        instance_fatal_error(lexer->instance, source_pos(lexer), "Expected base %llu digit, got '%c'", base, *lexer->stream);
        lexer->token.kind = TOK_ERROR;
        return false;
    }

    lexer->token.kind = TOK_INT;
    lexer->token.integer = result;
    lexer->token.flags = flags;

    return true;
}

static bool lex_real(Lexer* lexer)
{
    assert(lexer);

    const char* start = lexer->stream;

    // NOTE:
    // The scanning we do here is to leave the stream in the correct position when done.
    // The actual conversion is done by strtod/strtof.

    // All digits before the '.'
    while (isdigit(*lexer->stream)) lexer->stream += 1;

    if (*lexer->stream == '.') lexer->stream += 1;

    // All digits after the '.'
    while (isdigit(*lexer->stream)) lexer->stream += 1;

    // Scientific notation
    if (tolower(*lexer->stream) == 'e') {
        lexer->stream += 1;

        if (*lexer->stream == '+' || *lexer->stream == '-') {
            lexer->stream += 1;
        }

        if (!isdigit(*lexer->stream)) {

            instance_fatal_error(lexer->instance, source_pos(lexer), "Expected digit after float literal exponent, found '%c'.", *lexer->stream);
            lexer->token.kind = TOK_ERROR;
            return false;
        }

        // All digits after 'e' and optional '+'/'-'
        while (isdigit(*lexer->stream)) lexer->stream += 1;
    }

    Real_Value result;
    char* err;

    result.r32 = strtof(start, &err);
    if (result.r32 == 0.0 && err == start) {

        instance_fatal_error(lexer->instance, source_pos(lexer), "Convertion to float failed");
        lexer->token.kind = TOK_ERROR;

        return false;
    }
    if (result.r32 == HUGE_VALF || result.r32 == -HUGE_VALF) {

        instance_fatal_error(lexer->instance, source_pos(lexer), "Float literal overflow");
        lexer->token.kind = TOK_ERROR;

        return false;
    }
    if (result.r32 <= FLT_MIN && errno == ERANGE) {

        instance_fatal_error(lexer->instance, source_pos(lexer), "Float literal underflow");
        lexer->token.kind = TOK_ERROR;

        return false;
    }

    result.r64 = strtod(start, &err);
    if (result.r64 == 0.0 && err == start) {

        instance_fatal_error(lexer->instance, source_pos(lexer), "Convertion to double failed");
        lexer->token.kind = TOK_ERROR;

        return false;
    }
    if (result.r64 == HUGE_VAL || result.r64 == -HUGE_VAL) {

        instance_fatal_error(lexer->instance, source_pos(lexer), "Double literal overflow");
        lexer->token.kind = TOK_ERROR;

        return false;
    }
    if (result.r64 <= DBL_MIN && errno == ERANGE) {
        instance_fatal_error(lexer->instance, source_pos(lexer), "Double literal underflow");
        lexer->token.kind = TOK_ERROR;

        return false;
    }

    lexer->token.kind = TOK_REAL;
    lexer->token.real = result;

    return true;
}

}
