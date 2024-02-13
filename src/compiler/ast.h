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
        AST_Declaration* declaration;
        AST_Statement* statement;
        AST_Expression* expression;
        AST_Type_Spec* ts;
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
    AST_DECL_FLAG_NONE              = 0x000,
    AST_DECL_FLAG_PARAM             = 0x001,

    AST_DECL_FLAG_RESOLVED          = 0x002,
    AST_DECL_FLAG_TYPED             = 0x004,

    AST_DECL_FLAG_STORAGE_REQUIRED  = 0x008,
    AST_DECL_FLAG_FOREIGN           = 0x010,
    AST_DECL_FLAG_FOREIGN_VARARG    = 0x20,
    // Last = 0x80000000
};

struct AST_Declaration
{
    AST_Declaration_Kind kind;
    AST_Declaration_Flags flags;

    AST_Identifier* ident;
    Type* resolved_type;

    union {

        struct {
            AST_Type_Spec* type_spec;
            AST_Expression* init_expr;
            s64 index;
        } variable, constant;

        struct {
            Scope* scope;
            DArray<AST_Declaration *> fields;
        } structure;

        struct {
            DArray<AST_Declaration *> params;
            AST_Type_Spec* return_ts;
            DArray<AST_Statement *> body;
            DArray<AST_Declaration *> variables;
            DArray<AST_Expression *> temp_structs;
            DArray<AST_Node> wait_for_bytecode;

            Scope* scope;

        } function;
    };
};

enum class AST_Statement_Kind : u32
{
    INVALID,

    IMPORT,

    DECLARATION,

    ASSIGNMENT,
    ARITHMETIC_ASSIGNMENT,

    CALL,
    RETURN,

    IF,
    WHILE,
    FOR,

    BREAK,
    CONTINUE,

    BLOCK,

    ASSERT,
};

typedef u32 AST_Statement_Flags;
enum AST_Statement_Flag
{
    AST_STMT_FLAG_NONE      = 0x00,
    AST_STMT_FLAG_RESOLVED  = 0x01,
    AST_STMT_FLAG_TYPED     = 0x02,
};

struct AST_If_Block {
    AST_Expression* cond;
    AST_Statement* then;
};

struct AST_Statement
{
    AST_Statement_Kind kind;
    AST_Statement_Flags flags;

    union
    {
        String_Ref import_path;
        AST_Declaration* declaration;
        AST_Expression* call;
        AST_Expression* return_expr;

        struct {
            AST_Expression* lvalue;
            AST_Expression* rvalue;
        } assignment;

        struct {
            u32 op;
            AST_Expression* lvalue;
            AST_Expression* rvalue;
        } arithmetic_assignment;

        struct {
            DArray<AST_If_Block> blocks;
            AST_Statement * else_stmt;
        } if_stmt;

        struct {
            AST_Expression* cond;
            AST_Statement* stmt;
        } while_stmt;

        struct {
            AST_Statement* init;
            AST_Expression* cond;
            AST_Statement* step;
            AST_Statement* stmt;

            Scope* scope;
        } for_stmt;

        AST_Statement* loop_control_target;

        struct {
            DArray<AST_Statement *> statements;
            Scope* scope;
        } block;

        struct {
            AST_Expression* cond;
            AST_Expression* message;
        } assert_stmt;
    };
};

enum class AST_Expression_Kind : u32
{
    INVALID,

    IDENTIFIER,
    UNARY,
    BINARY,
    MEMBER,
    CALL,

    ADDRESS_OF,
    DEREF,

    CAST,

    COMPOUND,

    INTEGER_LITERAL,
    REAL_LITERAL,
    CHAR_LITERAL,
    BOOL_LITERAL,
    NULL_LITERAL,
    STRING_LITERAL,
};

typedef u32 AST_Expression_Flags;
enum AST_Expression_Flag : AST_Expression_Flags
{
    AST_EXPR_FLAG_NONE              = 0x0000,

    AST_EXPR_FLAG_RESOLVED          = 0x0001,
    AST_EXPR_FLAG_TYPED             = 0x0002,
    AST_EXPR_FLAG_CONST             = 0x0004,
    AST_EXPR_FLAG_LVALUE            = 0x0008,

    AST_EXPR_FLAG_HEX_LITERAL       = 0x0010,
    AST_EXPR_FLAG_BINARY_LITERAL    = 0x0020,
};

struct AST_Expression
{
    AST_Expression_Kind kind;
    AST_Expression_Flags flags;

    Type* resolved_type;

    union {
        AST_Identifier* identifier;

        struct
        {
            u32 op;
            AST_Expression* lhs;
            AST_Expression* rhs;
        } binary;

        struct
        {
            u32 op;
            AST_Expression* operand;
        } unary;

        struct
        {
            AST_Expression* base;
            AST_Identifier* member_name;
        } member;

        struct
        {
            AST_Expression* base;
            DArray<AST_Expression *> args;
        } call;

        struct {
            AST_Type_Spec* ts;
            AST_Expression* operand;
        } cast;

        struct {
            DArray<AST_Expression*> expressions;
        } compound;

        u64 integer_literal;
        Real_Value real_literal;
        char char_literal;
        bool bool_literal;
        Atom string_literal;
    };
};

enum class AST_Type_Spec_Kind : u32
{
    INVALID,
    IDENTIFIER,
    POINTER,
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

    Type* resolved_type;

    union {
        AST_Identifier* identifier;
        AST_Type_Spec *base;
    };
};

struct AST_Identifier
{
    Atom atom;

    AST_Declaration* decl;
};

NAPI bool operator==(const AST_Node &a, const AST_Node &b);

NAPI AST_Node ast_node(AST_Declaration* decl);
NAPI AST_Node ast_node(AST_Statement* stmt);
NAPI AST_Node ast_node(AST_Expression* expr);
NAPI AST_Node ast_node(AST_Type_Spec* expr);

NAPI Type* ast_node_type(const AST_Node& node);

NAPI AST_File* ast_file(Instance* inst, DArray<AST_Node> nodes);

NAPI AST_Declaration* ast_declaration(Instance* inst, AST_Declaration_Kind kind, AST_Identifier* ident);
NAPI AST_Declaration* ast_builtin_type_decl(Instance* inst, Type* type, const char* name);
NAPI AST_Declaration* ast_variable_declaration(Instance* inst, AST_Identifier* ident, AST_Type_Spec* ts, AST_Expression* init);
NAPI AST_Declaration* ast_struct_member_declaration(Instance* inst, AST_Identifier* ident, AST_Type_Spec* ts, AST_Expression* default_val);
NAPI AST_Declaration* ast_struct_declaration(Instance* inst, AST_Identifier* ident, DArray<AST_Declaration *> fields, Scope* scope);
NAPI AST_Declaration* ast_function_declaration(Instance* inst, AST_Identifier* ident, DArray<AST_Declaration *> arg_decls, DArray<AST_Statement *> body_stmts, AST_Type_Spec* return_ts, Scope* scope);

NAPI AST_Statement* ast_statement(Instance* inst, AST_Statement_Kind kind);
NAPI AST_Statement* ast_import_statement(Instance* inst, String_Ref path);
NAPI AST_Statement* ast_declaration_statement(Instance* inst, AST_Declaration* decl);
NAPI AST_Statement* ast_assignment_statement(Instance* inst, AST_Expression* lvalue, AST_Expression* rvalue);
NAPI AST_Statement* ast_arithmetic_assignment_statement(Instance* inst, u32 op, AST_Expression* lvalue, AST_Expression* rvalue);
NAPI AST_Statement* ast_call_expr_statement(Instance* inst, AST_Expression* call);
NAPI AST_Statement* ast_return_statement(Instance* inst, AST_Expression* expr);
NAPI AST_Statement* ast_if_statement(Instance* inst, DArray<AST_If_Block> if_blocks, AST_Statement* else_stmt);
NAPI AST_Statement* ast_while_statement(Instance* inst, AST_Expression* cond, AST_Statement* stmt);
NAPI AST_Statement* ast_for_statement(Instance* inst, AST_Statement* init, AST_Expression* cond, AST_Statement* step, AST_Statement* stmt, Scope* scope);
NAPI AST_Statement* ast_break_statement(Instance* inst);
NAPI AST_Statement* ast_continue_statement(Instance* inst);
NAPI AST_Statement* ast_block_statement(Instance* inst, DArray<AST_Statement *> stmts, Scope* scope);
NAPI AST_Statement* ast_assert_statement(Instance* inst, AST_Expression* cond, AST_Expression* message);

NAPI AST_Expression* ast_expression(Instance* inst, AST_Expression_Kind kind, AST_Expression_Flags flags = AST_EXPR_FLAG_NONE);
NAPI AST_Expression* ast_identifier_expression(Instance* inst, AST_Identifier* ident);
NAPI AST_Expression* ast_unary_expression(Instance* inst, u32 op, AST_Expression* operand);
NAPI AST_Expression* ast_binary_expression(Instance* inst, u32 op, AST_Expression* lhs, AST_Expression* rhs);
NAPI AST_Expression* ast_member_expression(Instance* inst, AST_Expression* base, AST_Identifier* member_name);
NAPI AST_Expression* ast_call_expression(Instance* inst, AST_Expression* base_expr, DArray<AST_Expression*> args);
NAPI AST_Expression* ast_address_of_expression(Instance* instance, AST_Expression* operand);
NAPI AST_Expression* ast_deref_expression(Instance* instance, AST_Expression* operand);
NAPI AST_Expression* ast_cast_expression(Instance* instance, AST_Type_Spec* ts, AST_Expression* operand);
NAPI AST_Expression* ast_compound_expression(Instance* inst, DArray<AST_Expression*> expressions);
NAPI AST_Expression* ast_integer_literal_expression(Instance* inst, u64 i);
NAPI AST_Expression* ast_real_literal_expression(Instance* inst, Real_Value rv);
NAPI AST_Expression* ast_char_literal_expression(Instance* inst, char c);
NAPI AST_Expression* ast_bool_literal_expression(Instance* inst, bool b);
NAPI AST_Expression* ast_null_literal_expression(Instance* inst);
NAPI AST_Expression* ast_string_literal_expression(Instance* inst, Atom atom);

NAPI AST_Type_Spec* ast_type_spec(Instance* inst, AST_Type_Spec_Kind kind);
NAPI AST_Type_Spec* ast_identifier_type_spec(Instance* inst, AST_Identifier* ident);
NAPI AST_Type_Spec* ast_pointer_type_spec(Instance* inst, AST_Type_Spec *base);

NAPI AST_Identifier* ast_identifier(Instance* inst, Atom atom);

}
