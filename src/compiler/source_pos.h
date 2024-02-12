#pragma once

#include <defines.h>
#include <containers/darray.h>

namespace Novo {

struct AST_Declaration;
struct AST_Expression;
struct AST_Identifier;
struct AST_Node;
struct AST_Statement;
struct AST_Type_Spec;
struct Instance;
struct Lexer;
struct Parser;
struct Token;

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

NAPI Source_Pos source_pos(const Lexer* lexer);
NAPI Source_Pos source_pos(const Parser* parser, const Token& tok);
NAPI Source_Pos source_pos(const Source_Pos& start, const Source_Pos& end);
NAPI Source_Pos source_pos(Instance* inst, AST_Identifier* ident);
NAPI Source_Pos source_pos(Instance* inst, AST_Declaration* decl);
NAPI Source_Pos source_pos(Instance* inst, AST_Statement* stmt);
NAPI Source_Pos source_pos(Instance* inst, AST_Expression* expr);
NAPI Source_Pos source_pos(Instance* inst, AST_Type_Spec* ts);
NAPI Source_Pos source_pos(Instance* inst, AST_Node &node);

NAPI Line_Info line_info(Array_Ref<u32> newline_offsets, u32 offset);

NAPI void save_source_pos(Instance* inst, AST_Identifier* ident, Source_Pos pos);
NAPI void save_source_pos(Instance* inst, AST_Declaration* decl, Source_Pos pos);
NAPI void save_source_pos(Instance* inst, AST_Statement* stmt, Source_Pos pos);
NAPI void save_source_pos(Instance* inst, AST_Expression* expr, Source_Pos pos);
NAPI void save_source_pos(Instance* inst, AST_Type_Spec* ts, Source_Pos pos);

}
