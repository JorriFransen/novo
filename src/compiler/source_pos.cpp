#include "source_pos.h"

#include "parser.h"

namespace Novo {

Source_Pos source_pos(const Lexer *lexer)
{
    return { (u32)lexer->import_index, lexer->token.offset, lexer->token.length };
}

Source_Pos source_pos(const Parser *parser, const Token &tok)
{
    return { (u32)parser->lexer->import_index, tok.offset, tok.length };
}

Line_Info line_info(Array_Ref<u32> newline_offsets, u32 offset)
{
    Line_Info result = { 0, offset + 1 };

    u32 line = 1;

    for (s64 i = 0; i < newline_offsets.count; i++, line++) {

        if (offset < newline_offsets[i]) {
            if (i == 0) break;
            result.line = line;
            result.offset = offset - newline_offsets[i - 1];
            return result;
        }
    }

    if (newline_offsets.count) {
        result.offset = offset - newline_offsets[newline_offsets.count - 1];
    }
    result.line = line;

    return result;
}

}
