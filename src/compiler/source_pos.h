#pragma once

#include <defines.h>

#include "lexer.h"

namespace Novo {

struct Parser;

struct Source_Pos
{
    u32 file_index;
    u32 offset;
    u32 length;
};

struct Line_Info
{
    u32 line;
    u32 offset;
};

NAPI Source_Pos source_pos(const Lexer *lexer);
NAPI Source_Pos source_pos(const Parser *parser, const Token &tok);

NAPI Line_Info line_info(Array_Ref<u32> newline_offsets, u32 offset);

}
