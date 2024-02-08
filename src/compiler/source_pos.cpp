#include "source_pos.h"

#include <containers/hash_table.h>

#include "instance.h"
#include "lexer.h"
#include "parser.h"

#include <assert.h>

namespace Novo {

Source_Pos source_pos(const Lexer* lexer)
{
    return { (u32)lexer->import_index, lexer->token.offset, lexer->token.length };
}

Source_Pos source_pos(const Parser* parser, const Token& tok)
{
    return { (u32)parser->lexer->import_index, tok.offset, tok.length };
}

Source_Pos source_pos(const Source_Pos& start, const Source_Pos& end)
{
    assert(start.file_index == end.file_index);
    assert(start.offset < end.offset);
    assert(start.offset + start.length <= end.offset);

    return { start.file_index, start.offset, end.offset - start.offset + end.length };
}

Source_Pos source_pos(Instance* inst, AST_Identifier* ident)
{
    Source_Pos result;
    bool found = hash_table_find(&inst->ident_positions, ident, &result);
    assert(found);
    return result;
}

Source_Pos source_pos(Instance* inst, AST_Declaration* decl)
{
    Source_Pos result;
    bool found = hash_table_find(&inst->decl_positions, decl, &result);
    assert(found);
    return result;
}

Source_Pos source_pos(Instance* inst, AST_Statement* stmt)
{
    Source_Pos result;
    bool found = hash_table_find(&inst->stmt_positions, stmt, &result);
    assert(found);
    return result;
}

Source_Pos source_pos(Instance* inst, AST_Expression* expr)
{
    Source_Pos result;
    bool found = hash_table_find(&inst->expr_positions, expr, &result);
    assert(found);
    return result;
}

Source_Pos source_pos(Instance* inst, AST_Type_Spec* ts)
{
    Source_Pos result;
    bool found = hash_table_find(&inst->ts_positions, ts, &result);
    assert(found);
    return result;
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

void save_source_pos(Instance* inst, AST_Identifier* ident, Source_Pos pos)
{
    hash_table_add(&inst->ident_positions, ident, pos);
}

void save_source_pos(Instance* inst, AST_Declaration* decl, Source_Pos pos)
{
    hash_table_add(&inst->decl_positions, decl, pos);
}

void save_source_pos(Instance* inst, AST_Statement* stmt, Source_Pos pos)
{
    hash_table_add(&inst->stmt_positions, stmt, pos);
}

void save_source_pos(Instance* inst, AST_Expression* expr, Source_Pos pos)
{
    hash_table_add(&inst->expr_positions, expr, pos);
}

void save_source_pos(Instance* inst, AST_Type_Spec* ts, Source_Pos pos)
{
    hash_table_add(&inst->ts_positions, ts, pos);
}

}
