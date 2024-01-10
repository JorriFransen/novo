#pragma once

#include <defines.h>
#include <nstring.h>

#include "instance.h"

namespace Novo {

struct AST_File;
struct AST_Declaration;
struct AST_Statement;
struct AST_Identifier;
struct AST_Type_Spec;
struct AST_Expression;

struct Parser
{
    Instance *instance;
    Lexer *lexer;
};

NAPI AST_File *parse_file(Instance *instance, const String_Ref file_path);

NAPI AST_Declaration *parse_declaration(Parser *parser, bool eat_semi);
NAPI AST_Declaration *parse_declaration(Parser *parser, AST_Identifier *ident, bool eat_semi);
NAPI AST_Declaration *parse_function_declaration(Parser *parser, AST_Identifier *ident);

NAPI AST_Statement *parse_statement(Parser *parser);
NAPI AST_Statement *parse_keyword_statement(Parser *parser);

NAPI AST_Expression *parse_leaf_expression(Parser *parser);
NAPI AST_Expression *parse_expression(Parser *parser, u64 min_prec = 0);

NAPI AST_Identifier *parse_identifier(Parser *parser);

NAPI AST_Type_Spec *parse_type_spec(Parser *parser);

NAPI bool expect_token(Parser *parser, Token_Kind kind);
NAPI bool expect_token(Parser *parser, char c);

NAPI bool match_token(Parser *parser, Token_Kind kind);
NAPI bool match_token(Parser *parser, char c);

NAPI bool is_token(Parser *parser, Token_Kind kind);
NAPI bool is_token(Parser *parser, char c);


}
