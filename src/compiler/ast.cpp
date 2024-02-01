#include "ast.h"

#include <memory/allocator.h>

#include "instance.h"
#include "source_pos.h"

#include <cassert>

namespace Novo {

bool operator==(const AST_Node &a, const AST_Node &b)
{
    return a.kind == b.kind && a.declaration == b.declaration;
}

AST_Node ast_node(AST_Declaration *decl)
{
    return AST_Node { AST_Node_Kind::DECLARATION, { .declaration = decl } };
}

AST_Node ast_node(AST_Statement *stmt)
{
    return AST_Node { AST_Node_Kind::STATEMENT, { .statement = stmt } };
}

AST_Node ast_node(AST_Expression *expr)
{
    return AST_Node { AST_Node_Kind::EXPRESSION, { .expression = expr } };
}

AST_Node ast_node(AST_Type_Spec *expr)
{
    return AST_Node { AST_Node_Kind::TYPE_SPEC, { .ts = expr } };
}

AST_File *ast_file(Instance *instance, DArray<AST_Node> nodes)
{
    auto result = allocate<AST_File>(&instance->ast_allocator);
    result->nodes = nodes;

    return result;
}

AST_Declaration *ast_declaration(Instance *instance, AST_Declaration_Kind kind, AST_Identifier *ident, u32 range_id)
{
    auto result = allocate<AST_Declaration>(&instance->ast_allocator);
    result->kind = kind;
    result->flags = AST_DECL_FLAG_NONE;
    result->ident = ident;
    result->resolved_type = nullptr;
    result->range_id = range_id;
    return result;
}

AST_Declaration *ast_builtin_type_decl(Instance *instance, Type *type, const char *name)
{
    auto ident = ast_identifier(instance, atom_get(name), 0);
    auto result = ast_declaration(instance, AST_Declaration_Kind::BUILTIN_TYPE, ident, 0);
    result->resolved_type = type;
    return result;
}

AST_Declaration *ast_variable_declaration(Instance *instance, AST_Identifier *ident, AST_Type_Spec *ts, AST_Expression *init, u32 range_id)
{
    auto result = ast_declaration(instance, AST_Declaration_Kind::VARIABLE, ident, range_id);
    result->variable.type_spec = ts;
    result->variable.init_expr = init;
    result->variable.index = -1;
    return result;
}

AST_Declaration *ast_struct_member_declaration(Instance *instance, AST_Identifier *ident, AST_Type_Spec *ts, AST_Expression *default_val, u32 range_id)
{
    auto result = ast_declaration(instance, AST_Declaration_Kind::STRUCT_MEMBER, ident, range_id);
    result->variable.type_spec = ts;
    result->variable.init_expr = default_val;
    result->variable.index = -1;
    return result;
}

AST_Declaration *ast_struct_declaration(Instance *instance, AST_Identifier *ident, DArray<AST_Declaration *> fields, Scope *scope, u32 range_id)
{
    auto result = ast_declaration(instance, AST_Declaration_Kind::STRUCT, ident, range_id);
    result->structure.scope = scope;
    result->structure.fields = fields;
    return result;
}

AST_Declaration *ast_function_declaration(Instance *instance, AST_Identifier *ident, DArray<AST_Declaration *> param_decls, DArray<AST_Statement *> body_stmts, AST_Type_Spec *return_ts, Scope *scope, u32 range_id)
{
    auto result = ast_declaration(instance, AST_Declaration_Kind::FUNCTION, ident, range_id);
    result->function.params = param_decls;
    result->function.body = body_stmts;
    result->function.return_ts = return_ts;
    result->function.scope = scope;
    darray_init(&instance->ast_allocator, &result->function.variables, 0);
    darray_init(&instance->ast_allocator, &result->function.temp_structs, 0);
    darray_init(&instance->ast_allocator, &result->function.wait_for_bytecode, 0);

    if (param_decls.count) {
        auto start = source_range_start(instance, param_decls[0]->range_id);
        auto end = source_range_end(instance, param_decls[param_decls.count - 1]->range_id);
        result->function.param_range_id = source_range(instance, start, end);
    } else {
        result->function.param_range_id = 0;
    }

    if (body_stmts.count) {
        auto start = source_range_start(instance, body_stmts[0]->range_id);
        auto end = source_range_end(instance, body_stmts[body_stmts.count - 1]->range_id);
        result->function.body_range_id = source_range(instance, start, end);
    } else {
        result->function.body_range_id = 0;
    }

    return result;
}

AST_Statement *ast_statement(Instance *instance, AST_Statement_Kind kind, u32 range_id)
{
    auto result = allocate<AST_Statement>(&instance->ast_allocator);
    result->kind = kind;
    result->flags = AST_STMT_FLAG_NONE;
    result->range_id = range_id;
    return result;
}

AST_Statement *ast_import_statement(Instance *instance, String_Ref path, u32 range_id)
{
    auto result = ast_statement(instance, AST_Statement_Kind::IMPORT, range_id);
    result->import_path = path;
    return result;
}

AST_Statement *ast_declaration_statement(Instance *instance, AST_Declaration *decl, u32 range_id)
{
    auto result = ast_statement(instance, AST_Statement_Kind::DECLARATION, range_id);
    result->declaration = decl;
    return result;
}

AST_Statement *ast_assignment_statement(Instance *inst, AST_Expression *lvalue, AST_Expression *rvalue, u32 range_id)
{
    auto result = ast_statement(inst, AST_Statement_Kind::ASSIGNMENT, range_id);
    result->assignment.lvalue = lvalue;
    result->assignment.rvalue = rvalue;
    return result;
}

AST_Statement *ast_call_expr_statement(Instance *instance, AST_Expression *call)
{
    assert(call->kind == AST_Expression_Kind::CALL);

    auto result = ast_statement(instance, AST_Statement_Kind::CALL, call->range_id);
    result->call = call;
    return result;
}

AST_Statement *ast_return_statement(Instance *instance, AST_Expression *expr, u32 range_id)
{
    auto result = ast_statement(instance, AST_Statement_Kind::RETURN, range_id);
    result->return_expr = expr;
    return result;
}

AST_Statement *ast_if_statement(Instance *instance, DArray<AST_If_Block> if_blocks, AST_Statement *else_stmt, u32 range_id)
{
    auto result = ast_statement(instance, AST_Statement_Kind::IF, range_id);
    result->if_stmt.blocks = if_blocks;
    result->if_stmt.else_stmt = else_stmt;
    return result;
}

AST_Statement *ast_while_statement(Instance *instance, AST_Expression *cond, AST_Statement *stmt, u32 range_id)
{
    auto result = ast_statement(instance, AST_Statement_Kind::WHILE, range_id);
    result->while_stmt.cond = cond;
    result->while_stmt.stmt = stmt;
    return result;
}

AST_Statement *ast_for_statement(Instance *instance, AST_Statement *init, AST_Expression *cond, AST_Statement *step, AST_Statement *stmt, Scope *scope, u32 range_id)
{
    auto result = ast_statement(instance, AST_Statement_Kind::FOR, range_id);
    result->for_stmt.init = init;
    result->for_stmt.cond = cond;
    result->for_stmt.step = step;
    result->for_stmt.stmt = stmt;
    result->for_stmt.scope = scope;
    return result;
}

AST_Statement *ast_block_statement(Instance *instance, DArray<AST_Statement *> stmts, Scope *scope, u32 range_id)
{
    auto result = ast_statement(instance, AST_Statement_Kind::BLOCK, range_id);
    result->block.statements = stmts;
    result->block.scope = scope;
    return result;
}

AST_Expression *ast_expression(Instance *instance, AST_Expression_Kind kind, u32 range_id)
{
    auto result = allocate<AST_Expression>(&instance->ast_allocator);
    result->kind = kind;
    result->flags = AST_EXPR_FLAG_NONE;
    result->resolved_type = nullptr;
    result->range_id = range_id;
    return result;
}

AST_Expression *ast_identifier_expression(Instance *instance, AST_Identifier *ident, u32 range_id)
{
    auto result = ast_expression(instance, AST_Expression_Kind::IDENTIFIER, range_id);
    result->identifier = ident;
    return result;
}

NAPI AST_Expression *ast_binary_expression(Instance *instance, u32 op, AST_Expression *lhs, AST_Expression *rhs, u32 range_id)
{
    auto result = ast_expression(instance, AST_Expression_Kind::BINARY, range_id);
    result->binary.op = op;
    result->binary.lhs = lhs;
    result->binary.rhs = rhs;
    return result;
}

AST_Expression *ast_member_expression(Instance *inst, AST_Expression *base, AST_Identifier *member_name, u32 range_id)
{
    auto result = ast_expression(inst, AST_Expression_Kind::MEMBER, range_id);
    result->member.base = base;
    result->member.member_name = member_name;
    return result;
}

AST_Expression *ast_call_expression(Instance *instance, AST_Expression *base_expr, DArray<AST_Expression *> args, u32 range_id)
{
    auto result = ast_expression(instance, AST_Expression_Kind::CALL, range_id);
    result->call.base = base_expr;
    result->call.args = args;
    return result;
}

AST_Expression *ast_integer_literal_expression(Instance *instance, u64 i, u32 range_id)
{
    auto result = ast_expression(instance, AST_Expression_Kind::INTEGER_LITERAL, range_id);
    result->integer_literal = i;
    return result;
}

AST_Expression *ast_real_literal_expression(Instance *instance, Real_Value rv, u32 range_id)
{
    auto result = ast_expression(instance, AST_Expression_Kind::REAL_LITERAL, range_id);
    result->real_literal = rv;
    return result;
}

AST_Expression *ast_char_literal_expression(Instance *instance, char c, u32 range_id)
{
    auto result = ast_expression(instance, AST_Expression_Kind::CHAR_LITERAL, range_id);
    result->char_literal = c;
    return result;
}

AST_Expression *ast_bool_literal_expression(Instance *instance, bool b, u32 range_id)
{
    auto result = ast_expression(instance, AST_Expression_Kind::BOOL_LITERAL, range_id);
    result->bool_literal = b;
    return result;
}

AST_Expression *ast_string_literal_expression(Instance *instance, Atom atom, u32 range_id)
{
    auto result = ast_expression(instance, AST_Expression_Kind::STRING_LITERAL, range_id);
    result->string_literal = atom;
    return result;
}

AST_Type_Spec *ast_type_spec(Instance *instance, AST_Type_Spec_Kind kind, u32 range_id)
{
    auto result = allocate<AST_Type_Spec>(&instance->ast_allocator);
    result->kind = kind;
    result->flags = AST_TS_FLAG_NONE;
    result->resolved_type = nullptr;
    result->range_id = range_id;
    return result;
}

AST_Type_Spec *ast_identifier_type_spec(Instance *instance, AST_Identifier *ident)
{
    auto result = ast_type_spec(instance, AST_Type_Spec_Kind::IDENTIFIER, ident->range_id);
    result->identifier = ident;
    return result;
}

AST_Identifier *ast_identifier(Instance *instance, Atom atom, u32 range_id)
{
    auto result = allocate<AST_Identifier>(&instance->ast_allocator);
    result->atom = atom;
    result->range_id = range_id;
    result->decl = nullptr;
    return result;
}

}
