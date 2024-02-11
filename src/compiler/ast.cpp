#include "ast.h"

#include <memory/allocator.h>

#include "instance.h"

#include <cassert>

namespace Novo {

bool operator==(const AST_Node &a, const AST_Node &b)
{
    return a.kind == b.kind && a.declaration == b.declaration;
}

AST_Node ast_node(AST_Declaration* decl)
{
    return AST_Node { AST_Node_Kind::DECLARATION, { .declaration = decl } };
}

AST_Node ast_node(AST_Statement* stmt)
{
    return AST_Node { AST_Node_Kind::STATEMENT, { .statement = stmt } };
}

AST_Node ast_node(AST_Expression* expr)
{
    return AST_Node { AST_Node_Kind::EXPRESSION, { .expression = expr } };
}

AST_Node ast_node(AST_Type_Spec* expr)
{
    return AST_Node { AST_Node_Kind::TYPE_SPEC, { .ts = expr } };
}

AST_File* ast_file(Instance* inst, DArray<AST_Node> nodes)
{
    auto result = allocate<AST_File>(&inst->ast_allocator);
    result->nodes = nodes;

    return result;
}

AST_Declaration* ast_declaration(Instance* inst, AST_Declaration_Kind kind, AST_Identifier* ident)
{
    auto result = allocate<AST_Declaration>(&inst->ast_allocator);
    result->kind = kind;
    result->flags = AST_DECL_FLAG_NONE;
    result->ident = ident;
    result->resolved_type = nullptr;
    return result;
}

AST_Declaration* ast_builtin_type_decl(Instance* inst, Type* type, const char* name)
{
    auto ident = ast_identifier(inst, atom_get(name));
    auto result = ast_declaration(inst, AST_Declaration_Kind::BUILTIN_TYPE, ident);
    result->resolved_type = type;
    return result;
}

AST_Declaration* ast_variable_declaration(Instance* inst, AST_Identifier* ident, AST_Type_Spec* ts, AST_Expression* init)
{
    assert(ts || init);

    auto result = ast_declaration(inst, AST_Declaration_Kind::VARIABLE, ident);
    result->variable.type_spec = ts;
    result->variable.init_expr = init;
    result->variable.index = -1;
    return result;
}

AST_Declaration* ast_struct_member_declaration(Instance* inst, AST_Identifier* ident, AST_Type_Spec* ts, AST_Expression* default_val)
{
    auto result = ast_declaration(inst, AST_Declaration_Kind::STRUCT_MEMBER, ident);
    result->variable.type_spec = ts;
    result->variable.init_expr = default_val;
    result->variable.index = -1;
    return result;
}

AST_Declaration* ast_struct_declaration(Instance* inst, AST_Identifier* ident, DArray<AST_Declaration*> fields, Scope* scope)
{
    auto result = ast_declaration(inst, AST_Declaration_Kind::STRUCT, ident);
    result->structure.scope = scope;
    result->structure.fields = fields;
    return result;
}

AST_Declaration* ast_function_declaration(Instance* inst, AST_Identifier* ident, DArray<AST_Declaration*> param_decls, DArray<AST_Statement*> body_stmts, AST_Type_Spec* return_ts, Scope* scope)
{
    auto result = ast_declaration(inst, AST_Declaration_Kind::FUNCTION, ident);
    result->function.params = param_decls;
    result->function.body = body_stmts;
    result->function.return_ts = return_ts;
    result->function.scope = scope;
    darray_init(&inst->ast_allocator, &result->function.variables, 0);
    darray_init(&inst->ast_allocator, &result->function.temp_structs, 0);
    darray_init(&inst->ast_allocator, &result->function.wait_for_bytecode, 0);

    return result;
}

AST_Statement* ast_statement(Instance* inst, AST_Statement_Kind kind)
{
    auto result = allocate<AST_Statement>(&inst->ast_allocator);
    result->kind = kind;
    result->flags = AST_STMT_FLAG_NONE;
    return result;
}

AST_Statement* ast_import_statement(Instance* inst, String_Ref path)
{
    auto result = ast_statement(inst, AST_Statement_Kind::IMPORT);
    result->import_path = path;
    return result;
}

AST_Statement* ast_declaration_statement(Instance* inst, AST_Declaration* decl)
{
    auto result = ast_statement(inst, AST_Statement_Kind::DECLARATION);
    result->declaration = decl;
    return result;
}

AST_Statement* ast_assignment_statement(Instance* inst, AST_Expression* lvalue, AST_Expression* rvalue)
{
    auto result = ast_statement(inst, AST_Statement_Kind::ASSIGNMENT);
    result->assignment.lvalue = lvalue;
    result->assignment.rvalue = rvalue;
    return result;
}

AST_Statement* ast_arithmetic_assignment_statement(Instance* inst, u32 op, AST_Expression* lvalue, AST_Expression* rvalue)
{
    auto result = ast_statement(inst, AST_Statement_Kind::ARITHMETIC_ASSIGNMENT);
    result->arithmetic_assignment.op = op;
    result->arithmetic_assignment.lvalue = lvalue;
    result->arithmetic_assignment.rvalue = rvalue;
    return result;
}

AST_Statement* ast_call_expr_statement(Instance* inst, AST_Expression* call)
{
    assert(call->kind == AST_Expression_Kind::CALL);

    auto result = ast_statement(inst, AST_Statement_Kind::CALL);
    result->call = call;
    return result;
}

AST_Statement* ast_return_statement(Instance* inst, AST_Expression* expr)
{
    auto result = ast_statement(inst, AST_Statement_Kind::RETURN);
    result->return_expr = expr;
    return result;
}

AST_Statement* ast_if_statement(Instance* inst, DArray<AST_If_Block> if_blocks, AST_Statement* else_stmt)
{
    assert(if_blocks.count);

    auto result = ast_statement(inst, AST_Statement_Kind::IF);
    result->if_stmt.blocks = if_blocks;
    result->if_stmt.else_stmt = else_stmt;
    return result;
}

AST_Statement* ast_while_statement(Instance* inst, AST_Expression* cond, AST_Statement* stmt)
{
    auto result = ast_statement(inst, AST_Statement_Kind::WHILE);
    result->while_stmt.cond = cond;
    result->while_stmt.stmt = stmt;
    return result;
}

AST_Statement* ast_for_statement(Instance* inst, AST_Statement* init, AST_Expression* cond, AST_Statement* step, AST_Statement* stmt, Scope* scope)
{
    auto result = ast_statement(inst, AST_Statement_Kind::FOR);
    result->for_stmt.init = init;
    result->for_stmt.cond = cond;
    result->for_stmt.step = step;
    result->for_stmt.stmt = stmt;
    result->for_stmt.scope = scope;
    return result;
}

AST_Statement* ast_break_statement(Instance* inst)
{
    auto result = ast_statement(inst, AST_Statement_Kind::BREAK);
    result->loop_control_target = nullptr;
    return result;
}

AST_Statement* ast_continue_statement(Instance* inst)
{
    auto result = ast_statement(inst, AST_Statement_Kind::CONTINUE);
    result->loop_control_target = nullptr;
    return result;
}

AST_Statement* ast_block_statement(Instance* inst, DArray<AST_Statement*> stmts, Scope* scope)
{
    auto result = ast_statement(inst, AST_Statement_Kind::BLOCK);
    result->block.statements = stmts;
    result->block.scope = scope;
    return result;
}

AST_Statement* ast_assert_statement(Instance* inst, AST_Expression* cond, AST_Expression* message)
{
    auto result = ast_statement(inst, AST_Statement_Kind::ASSERT);
    result->assert_stmt.cond = cond;
    result->assert_stmt.message = message;
    return result;
}

AST_Expression* ast_expression(Instance* inst, AST_Expression_Kind kind, AST_Expression_Flags flags/*=AST_EXPR_FLAG_NONE*/)
{
    auto result = allocate<AST_Expression>(&inst->ast_allocator);
    result->kind = kind;
    result->flags = flags;
    result->resolved_type = nullptr;
    return result;
}

AST_Expression* ast_identifier_expression(Instance* inst, AST_Identifier* ident)
{
    auto result = ast_expression(inst, AST_Expression_Kind::IDENTIFIER);
    result->identifier = ident;
    return result;
}

NAPI AST_Expression* ast_binary_expression(Instance* inst, u32 op, AST_Expression* lhs, AST_Expression* rhs)
{
    auto result = ast_expression(inst, AST_Expression_Kind::BINARY);
    result->binary.op = op;
    result->binary.lhs = lhs;
    result->binary.rhs = rhs;
    return result;
}

AST_Expression* ast_member_expression(Instance* inst, AST_Expression* base, AST_Identifier* member_name)
{
    auto result = ast_expression(inst, AST_Expression_Kind::MEMBER);
    result->member.base = base;
    result->member.member_name = member_name;
    return result;
}

AST_Expression* ast_call_expression(Instance* inst, AST_Expression* base_expr, DArray<AST_Expression*> args)
{
    auto result = ast_expression(inst, AST_Expression_Kind::CALL);
    result->call.base = base_expr;
    result->call.args = args;
    return result;
}

AST_Expression *ast_address_of_expression(Instance* instance, AST_Expression* operand)
{
    auto result = ast_expression(instance, AST_Expression_Kind::ADDRESS_OF);
    result->unary.operand = operand;
    return result;
}

AST_Expression *ast_deref_expression(Instance* instance, AST_Expression* operand)
{
    auto result = ast_expression(instance, AST_Expression_Kind::DEREF);
    result->unary.operand = operand;
    return result;
}

AST_Expression* ast_cast_expression(Instance* instance, AST_Type_Spec* ts, AST_Expression* operand)
{
    auto result = ast_expression(instance, AST_Expression_Kind::CAST);
    result->cast.ts = ts;
    result->cast.operand = operand;
    return result;
}

AST_Expression* ast_compound_expression(Instance* inst, DArray<AST_Expression*> expressions)
{
    auto result = ast_expression(inst, AST_Expression_Kind::COMPOUND);
    result->compound.expressions = expressions;
    return result;
}

AST_Expression* ast_integer_literal_expression(Instance* inst, u64 i)
{
    auto result = ast_expression(inst, AST_Expression_Kind::INTEGER_LITERAL, AST_EXPR_FLAG_CONST);
    result->integer_literal = i;
    return result;
}

AST_Expression* ast_real_literal_expression(Instance* inst, Real_Value rv)
{
    auto result = ast_expression(inst, AST_Expression_Kind::REAL_LITERAL, AST_EXPR_FLAG_CONST);
    result->real_literal = rv;
    return result;
}

AST_Expression* ast_char_literal_expression(Instance* inst, char c)
{
    auto result = ast_expression(inst, AST_Expression_Kind::CHAR_LITERAL, AST_EXPR_FLAG_CONST);
    result->char_literal = c;
    return result;
}

AST_Expression* ast_bool_literal_expression(Instance* inst, bool b)
{
    auto result = ast_expression(inst, AST_Expression_Kind::BOOL_LITERAL, AST_EXPR_FLAG_CONST);
    result->bool_literal = b;
    return result;
}

AST_Expression* ast_string_literal_expression(Instance* inst, Atom atom)
{
    auto result = ast_expression(inst, AST_Expression_Kind::STRING_LITERAL, AST_EXPR_FLAG_CONST);
    result->string_literal = atom;
    return result;
}

AST_Type_Spec* ast_type_spec(Instance* inst, AST_Type_Spec_Kind kind)
{
    auto result = allocate<AST_Type_Spec>(&inst->ast_allocator);
    result->kind = kind;
    result->flags = AST_TS_FLAG_NONE;
    result->resolved_type = nullptr;
    return result;
}

AST_Type_Spec* ast_identifier_type_spec(Instance* inst, AST_Identifier* ident)
{
    auto result = ast_type_spec(inst, AST_Type_Spec_Kind::IDENTIFIER);
    result->identifier = ident;
    return result;
}

AST_Type_Spec* ast_pointer_type_spec(Instance* inst, AST_Type_Spec *base)
{
    auto result = ast_type_spec(inst, AST_Type_Spec_Kind::POINTER);
    result->base = base;
    return result;
}

AST_Identifier* ast_identifier(Instance* inst, Atom atom)
{
    auto result = allocate<AST_Identifier>(&inst->ast_allocator);
    result->atom = atom;
    result->decl = nullptr;
    return result;
}

}
