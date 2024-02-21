#pragma once

#include <defines.h>
#include <nstring.h>

#include "ast.h"
#include "atom.h"
#include "lexer.h"

namespace Novo {

enum Token_Kind : u32;

struct Instance;
struct Scope;
struct Lexer;

struct Parser
{
    Instance* instance;
    Lexer lexer;

    Temp_Allocator_Mark file_read_mark;
    String_Ref cwd;

    s64 next_index_in_function;
    bool parsing_function_body;

    // TODO: stack for nested functions
    Atom current_function_name;

    AST_Expression_Flags new_expr_flags;
};

NAPI Parser parser_create(Instance* inst, const String_Ref file_path, s64 import_index);
NAPI Parser parser_create(Instance* inst, const String_Ref name, const String_Ref content, s64 import_index, u32 offset);

NAPI AST_File* parse_file(Instance* instance, const String_Ref file_path, s64 import_index);
NAPI DArray<AST_Node> parse_string(Instance* inst, const String_Ref name, const String_Ref content, s64 import_index, u32 offset);
NAPI DArray<AST_Node> parse_nodes(Instance* inst, Parser* parser);

NAPI AST_Declaration* parse_declaration(Parser* parser, Scope* scope, bool eat_semi);
NAPI AST_Declaration* parse_declaration(Parser* parser, AST_Identifier* ident, Scope* scope, bool eat_semi);
NAPI AST_Declaration* parse_struct_declaration(Parser* parser, AST_Identifier* ident, Scope* scope);
NAPI AST_Declaration* parse_function_declaration(Parser* parser, AST_Identifier* ident, Scope* scope);

NAPI AST_Statement* parse_statement(Parser* parser, Scope* scope, bool eat_semi);
NAPI AST_Statement* parse_keyword_statement(Parser* parser, Scope* scope);

NAPI AST_Expression* parse_leaf_expression(Parser* parser);
NAPI AST_Expression* parse_expression(Parser* parser, u64 min_prec = 0);

NAPI AST_Identifier* parse_identifier(Parser* parser);

NAPI AST_Type_Spec* parse_type_spec(Parser* parser);

NAPI bool expect_token_internal(Parser* parser, Token_Kind kind);
NAPI bool expect_token_internal(Parser* parser, char c);

NAPI bool match_token(Parser* parser, Token_Kind kind);
NAPI bool match_token(Parser* parser, char c);
NAPI bool match_name(Parser* parser, const char* name);
NAPI bool match_keyword(Parser* parser, Atom kw_atom);

NAPI bool is_token(Parser* parser, Token_Kind kind);
NAPI bool is_token(Parser* parser, char c);
NAPI bool is_keyword(Parser* parser, Atom kw_atom);

NAPI u64 get_precedence(Token_Kind op);


}
