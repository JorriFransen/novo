#include "ast.h"

#include "instance.h"
#include "parser.h"

#include <memory/allocator.h>

namespace Novo {

AST_File *ast_file(Instance *instance, DArray<AST_Declaration *> decls)
{
    auto result = allocate<AST_File>(&instance->ast_allocator);
    result->declarations = decls;
    return result;
}

AST_Declaration *ast_declaration(Instance *instance, AST_Declaration_Kind kind, AST_Identifier *ident)
{
    auto result = allocate<AST_Declaration>(&instance->ast_allocator);
    result->kind = kind;
    result->ident = ident;
    return result;
}

AST_Declaration *ast_variable_declaration(Instance *instance, AST_Identifier *ident, AST_Type_Spec *ts, AST_Expression *init)
{
    auto result = ast_declaration(instance, AST_Declaration_Kind::VARIABLE, ident);
    result->variable.ts = ts;
    result->variable.init_expr = init;
    return result;
}

AST_Declaration *ast_function_declaration(Instance *instance, AST_Identifier *ident, DArray<AST_Declaration *> param_decls, DArray<AST_Statement *> body_stmts, AST_Type_Spec *return_ts)
{
    auto result = ast_declaration(instance, AST_Declaration_Kind::FUNCTION, ident);
    result->function.params = param_decls;
    result->function.body = body_stmts;
    result->function.return_ts = return_ts;
    return result;
}


AST_Statement *ast_statement(Instance *instance, AST_Statement_Kind kind)
{
    auto result = allocate<AST_Statement>(&instance->ast_allocator);
    result->kind = kind;
    return result;
}

AST_Statement *ast_declaration_statement(Instance *instance, AST_Declaration *decl)
{
    auto result = ast_statement(instance, AST_Statement_Kind::DECLARATION);
    result->declaration = decl;
    return result;
}

AST_Statement *ast_return_statement(Instance *instance, AST_Expression *expr)
{
    auto result = ast_statement(instance, AST_Statement_Kind::RETURN);
    result->return_expr = expr;
    return result;
}

AST_Expression *ast_expression(Instance *instance, AST_Expression_Kind kind)
{
    auto result = allocate<AST_Expression>(&instance->ast_allocator);
    result->kind = kind;
    return result;
}

AST_Expression *ast_identifier_expression(Instance *instance, AST_Identifier *ident)
{
    auto result = ast_expression(instance, AST_Expression_Kind::IDENTIFIER);
    result->identifier = ident;
    return result;
}

NAPI AST_Expression *ast_binary_expression(Instance *instance, char op, AST_Expression *lhs, AST_Expression *rhs)
{
    auto result = ast_expression(instance, AST_Expression_Kind::BINARY);
    result->binary.op = op;
    result->binary.lhs = lhs;
    result->binary.rhs = rhs;
    return result;
}

AST_Expression *ast_integer_literal_expression(Instance *instance, u64 i)
{
    auto result = ast_expression(instance, AST_Expression_Kind::INTEGER_LITERAL);
    result->integer_literal = i;
    return result;
}

AST_Expression *ast_char_literal_expression(Instance *instance, char c)
{
    auto result = ast_expression(instance, AST_Expression_Kind::CHAR_LITERAL);
    result->char_literal = c;
    return result;
}

AST_Expression *ast_string_literal_expression(Instance *instance, Atom atom)
{
    auto result = ast_expression(instance, AST_Expression_Kind::STRING_LITERAL);
    result->string_literal = atom;
    return result;
}

AST_Type_Spec *ast_type_spec(Instance *instance, AST_Type_Spec_Kind kind)
{
    auto result = allocate<AST_Type_Spec>(&instance->ast_allocator);
    result->kind = kind;
    return result;
}

AST_Type_Spec *ast_identifier_type_spec(Instance *instance, AST_Identifier *ident)
{
    auto result = ast_type_spec(instance, AST_Type_Spec_Kind::IDENTIFIER);
    result->identifier = ident;
    return result;
}

AST_Identifier *ast_identifier(Instance *instance, Atom atom)
{
    auto result = allocate<AST_Identifier>(&instance->ast_allocator);
    result->atom = atom;
    return result;
}

}
