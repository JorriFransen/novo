#pragma once

#include <containers/darray.h>
#include <defines.h>
#include <nstring.h>

#include "atom.h"

namespace Novo {

struct AST_Declaration;
struct AST_Expression;
struct AST_Identifier;
struct AST_Statement;
struct AST_Type_Spec;
struct Instance;
struct Scope;
struct Type;

enum class AST_Node_Kind
{
    INVALID,
    DECLARATION,
    STATEMENT,
    EXPRESSION,
    TYPE_SPEC,
};

struct AST_Node
{
    AST_Node_Kind kind;

    union
    {
        AST_Declaration *declaration;
        AST_Statement *statement;
        AST_Expression *expression;
        AST_Type_Spec *ts;
    };
};

struct AST_File
{
    DArray<AST_Node> nodes;
};

enum class AST_Declaration_Kind : u32
{
    INVALID,
    VARIABLE,
    STRUCT_MEMBER,
    STRUCT,
    FUNCTION,

    BUILTIN_TYPE,
};

typedef u32 AST_Declaration_Flags;
enum AST_Declaration_Flag : AST_Declaration_Flags
{
    AST_DECL_FLAG_NONE             = 0x00,
    AST_DECL_FLAG_PARAM            = 0x01,

    AST_DECL_FLAG_RESOLVED         = 0x02,
    AST_DECL_FLAG_TYPED            = 0x04,

    AST_DECL_FLAG_STORAGE_REQUIRED = 0x08,
    // Last = 0x80000000
};

struct AST_Declaration
{
    AST_Declaration_Kind kind;
    AST_Declaration_Flags flags;

    AST_Identifier *ident;
    Type *resolved_type;

    union {

        struct {
            AST_Type_Spec *type_spec;
            AST_Expression *init_expr;
            s64 index;
        } variable, constant;

        struct {
            Scope *scope;
            DArray<AST_Declaration *> fields;
        } structure;

        struct {
            DArray<AST_Declaration *> params;
            AST_Type_Spec *return_ts;
            DArray<AST_Statement *> body;
            DArray<AST_Declaration *> variables;
            DArray<AST_Expression *> temp_structs;
            DArray<AST_Node> wait_for_bytecode;

            Scope *scope;

            u32 param_range_id;
            u32 body_range_id;
        } function;
    };

    u32 range_id;
};

enum class AST_Statement_Kind : u32
{
    INVALID,

    IMPORT,

    DECLARATION,

    ASSIGNMENT,

    CALL,
    RETURN,

    IF,
    WHILE,

    BLOCK,
};

typedef u32 AST_Statement_Flags;
enum AST_Statement_Flag
{
    AST_STMT_FLAG_NONE = 0x00,
    AST_STMT_FLAG_RESOLVED = 0x01,
    AST_STMT_FLAG_TYPED    = 0x02,
};

struct AST_If_Block {
    AST_Expression *cond;
    AST_Statement *then;
};

struct AST_Statement
{
    AST_Statement_Kind kind;
    AST_Statement_Flags flags;

    union
    {
        String_Ref import_path;
        AST_Declaration *declaration;
        AST_Expression *call;
        AST_Expression *return_expr;

        struct {
            AST_Expression *lvalue;
            AST_Expression *rvalue;
        } assignment;

        struct {
            DArray<AST_If_Block> blocks;
            AST_Statement * else_stmt;
        } if_stmt;

        struct {
            AST_Expression *cond;
            AST_Statement *stmt;
        } while_stmt;

        struct {
            DArray<AST_Statement *> statements;
            Scope *scope;
        } block;
    };

    u32 range_id;
};

enum class AST_Expression_Kind : u32
{
    INVALID,

    IDENTIFIER,
    BINARY,
    MEMBER,
    CALL,

    INTEGER_LITERAL,
    REAL_LITERAL,
    CHAR_LITERAL,
    BOOL_LITERAL,
    STRING_LITERAL,
};

typedef u32 AST_Expression_Flags;
enum AST_Expression_Flag
{
    AST_EXPR_FLAG_NONE     = 0x00,
    AST_EXPR_FLAG_RESOLVED = 0x01,
    AST_EXPR_FLAG_TYPED    = 0x02,
};

struct AST_Expression
{
    AST_Expression_Kind kind;
    AST_Expression_Flags flags;

    Type *resolved_type;

    union {
        AST_Identifier *identifier;

        struct
        {
            u32 op;
            AST_Expression *lhs;
            AST_Expression *rhs;
        } binary;

        struct
        {
            AST_Expression *base;
            AST_Identifier *member_name;
        } member;

        struct
        {
            AST_Expression *base;
            DArray<AST_Expression *> args;
        } call;

        u64 integer_literal;
        Real_Value real_literal;
        char char_literal;
        bool bool_literal;
        Atom string_literal;
    };

    u32 range_id;
};

enum class AST_Type_Spec_Kind : u32
{
    INVALID,
    IDENTIFIER,
};

typedef u32 AST_Type_Spec_Flags;
enum AST_Type_Spec_Flag
{
    AST_TS_FLAG_NONE     = 0x00,
    AST_TS_FLAG_RESOLVED = 0x01,
    AST_TS_FLAG_TYPED    = 0x02,
};

struct AST_Type_Spec
{
    AST_Type_Spec_Kind kind;
    AST_Type_Spec_Flags flags;

    Type *resolved_type;

    union {
        AST_Identifier *identifier;
    };

    u32 range_id;
};

struct AST_Identifier
{
    Atom atom;
    u32 range_id;

    AST_Declaration *decl;
};

NAPI bool operator==(const AST_Node &a, const AST_Node &b);

NAPI AST_Node ast_node(AST_Declaration *decl);
NAPI AST_Node ast_node(AST_Statement *stmt);
NAPI AST_Node ast_node(AST_Expression *expr);
NAPI AST_Node ast_node(AST_Type_Spec *expr);

NAPI AST_File *ast_file(Instance *instance, DArray<AST_Node> nodes);

NAPI AST_Declaration *ast_declaration(Instance *instance, AST_Declaration_Kind kind, AST_Identifier *ident, u32 range_id);
NAPI AST_Declaration *ast_builtin_type_decl(Instance *instance, Type *type, const char *name);
NAPI AST_Declaration *ast_variable_declaration(Instance *instance, AST_Identifier *ident, AST_Type_Spec *ts, AST_Expression *init, u32 range_id);
NAPI AST_Declaration *ast_struct_member_declaration(Instance *instance, AST_Identifier *ident, AST_Type_Spec *ts, AST_Expression *default_val, u32 range_id);
NAPI AST_Declaration *ast_struct_declaration(Instance *instance, AST_Identifier *ident, DArray<AST_Declaration *> fields, Scope *scope, u32 range_id);
NAPI AST_Declaration *ast_function_declaration(Instance *instance, AST_Identifier *ident, DArray<AST_Declaration *> arg_decls, DArray<AST_Statement *> body_stmts, AST_Type_Spec *return_ts, Scope *scope, u32 range_id);

NAPI AST_Statement *ast_statement(Instance *instance, AST_Statement_Kind kind, u32 range_id);
NAPI AST_Statement *ast_import_statement(Instance *instance, String_Ref path, u32 range_id);
NAPI AST_Statement *ast_declaration_statement(Instance *instance, AST_Declaration *decl, u32 range_id);
NAPI AST_Statement *ast_assignment_statement(Instance *inst, AST_Expression *lvalue, AST_Expression *rvalue, u32 range_id);
NAPI AST_Statement *ast_call_expr_statement(Instance *instance, AST_Expression *call);
NAPI AST_Statement *ast_return_statement(Instance *instance, AST_Expression *expr, u32 range_id);
NAPI AST_Statement *ast_if_statement(Instance *instance, DArray<AST_If_Block> if_blocks, AST_Statement *else_stmt, u32 range_id);
NAPI AST_Statement *ast_while_statement(Instance *instance, AST_Expression *cond, AST_Statement *stmt, u32 range_id);
NAPI AST_Statement *ast_block_statement(Instance *instance, DArray<AST_Statement *> stmts, Scope *scope, u32 range_id);

NAPI AST_Expression *ast_expression(Instance *instance, AST_Expression_Kind kind, u32 range_id);
NAPI AST_Expression *ast_identifier_expression(Instance *instance, AST_Identifier *ident, u32 range_id);
NAPI AST_Expression *ast_binary_expression(Instance *instance, u32 op, AST_Expression *lhs, AST_Expression *rhs, u32 range_id);
NAPI AST_Expression *ast_member_expression(Instance *inst, AST_Expression *base, AST_Identifier *member_name, u32 range_id);
NAPI AST_Expression *ast_call_expression(Instance *instance, AST_Expression *base_expr, DArray<AST_Expression *> args, u32 range_id);
NAPI AST_Expression *ast_integer_literal_expression(Instance *instance, u64 i, u32 range_id);
NAPI AST_Expression *ast_real_literal_expression(Instance *instance, Real_Value rv, u32 range_id);
NAPI AST_Expression *ast_char_literal_expression(Instance *instance, char c, u32 range_id);
NAPI AST_Expression *ast_bool_literal_expression(Instance *instance, bool b, u32 range_id);
NAPI AST_Expression *ast_string_literal_expression(Instance *instance, Atom atom, u32 range_id);

NAPI AST_Type_Spec *ast_type_spec(Instance *instance, AST_Type_Spec_Kind kind, u32 range_id);
NAPI AST_Type_Spec *ast_identifier_type_spec(Instance *instance, AST_Identifier *ident);

NAPI AST_Identifier *ast_identifier(Instance *instance, Atom atom, u32 range_id);
}
