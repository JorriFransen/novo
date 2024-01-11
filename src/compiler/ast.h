#pragma once

#include "atom.h"

#include <containers/darray.h>

namespace Novo {

struct Instance;
struct AST_Declaration;
struct AST_Statement;
struct AST_Expression;
struct AST_Type_Spec;
struct AST_Identifier;

struct AST_File
{
    DArray<AST_Declaration *> declarations;
};

enum class AST_Declaration_Kind
{
    INVALID,
    VARIABLE,
    FUNCTION,
};

struct AST_Declaration
{
    AST_Declaration_Kind kind;

    AST_Identifier *ident;

    union {

        struct {
            AST_Type_Spec *ts;
            AST_Expression *init_expr;
        } variable, constant;

        struct {
            DArray<AST_Declaration *> params;
            DArray<AST_Statement *> body;
            AST_Type_Spec *return_ts;

            u32 param_range_id;
            u32 body_range_id;
        } function;
    };

    u32 range_id;
};

enum class AST_Statement_Kind
{
    INVALID,
    DECLARATION,
    RETURN,
};

struct AST_Statement
{
    AST_Statement_Kind kind;

    union
    {
        AST_Declaration *declaration;
        AST_Expression *return_expr;
    };

    u32 range_id;
};

enum class AST_Expression_Kind
{
    INVALID,

    IDENTIFIER,
    BINARY,

    INTEGER_LITERAL,
    CHAR_LITERAL,
    STRING_LITERAL,
};

struct AST_Expression
{
    AST_Expression_Kind kind;

    union {
        AST_Identifier *identifier;

        struct
        {
            char op;
            AST_Expression *lhs;
            AST_Expression *rhs;
        } binary;

        u64 integer_literal;
        char char_literal;
        Atom string_literal;
    };

    u32 range_id;
};

enum class AST_Type_Spec_Kind
{
    INVALID,
    IDENTIFIER,
};

struct AST_Type_Spec
{
    AST_Type_Spec_Kind kind;

    union {
        AST_Identifier *identifier;
    };

    u32 range_id;
};

struct AST_Identifier
{
    Atom atom;
    u32 range_id;
};

NAPI AST_File *ast_file(Instance *instance, DArray<AST_Declaration *> decls);

NAPI AST_Declaration *ast_declaration(Instance *instance, AST_Declaration_Kind kind, AST_Identifier *ident, u32 range_id);
NAPI AST_Declaration *ast_variable_declaration(Instance *instance, AST_Identifier *ident, AST_Type_Spec *ts, AST_Expression *init, u32 range_id);
NAPI AST_Declaration *ast_function_declaration(Instance *instance, AST_Identifier *ident, DArray<AST_Declaration *> arg_decls, DArray<AST_Statement *> body_stmts, AST_Type_Spec *return_ts, u32 range_id);

NAPI AST_Statement *ast_statement(Instance *instance, AST_Statement_Kind kind, u32 range_id);
NAPI AST_Statement *ast_declaration_statement(Instance *instance, AST_Declaration *decl, u32 range_id);
NAPI AST_Statement *ast_return_statement(Instance *instance, AST_Expression *expr, u32 range_id);

NAPI AST_Expression *ast_expression(Instance *instance, AST_Expression_Kind kind, u32 range_id);
NAPI AST_Expression *ast_identifier_expression(Instance *instance, AST_Identifier *ident, u32 range_id);
NAPI AST_Expression *ast_binary_expression(Instance *instance, char op, AST_Expression *lhs, AST_Expression *rhs, u32 range_id);
NAPI AST_Expression *ast_integer_literal_expression(Instance *instance, u64 i, u32 range_id);
NAPI AST_Expression *ast_char_literal_expression(Instance *instance, char c, u32 range_id);
NAPI AST_Expression *ast_string_literal_expression(Instance *instance, Atom atom, u32 range_id);

NAPI AST_Type_Spec *ast_type_spec(Instance *instance, AST_Type_Spec_Kind kind, u32 range_id);
NAPI AST_Type_Spec *ast_identifier_type_spec(Instance *instance, AST_Identifier *ident);

NAPI AST_Identifier *ast_identifier(Instance *instance, Atom atom, u32 range_id);
}
