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

AST_Declaration* ast_declaration(Instance* inst, AST_Declaration_Kind kind, AST_Identifier* ident, u32 range_id)
{
    auto result = allocate<AST_Declaration>(&inst->ast_allocator);
    result->kind = kind;
    result->flags = AST_DECL_FLAG_NONE;
    result->ident = ident;
    result->resolved_type = nullptr;
    result->range_id = range_id;
    return result;
}

AST_Declaration* ast_builtin_type_decl(Instance* inst, Type* type, const char* name)
{
    auto ident = ast_identifier(inst, atom_get(name), 0);
    auto result = ast_declaration(inst, AST_Declaration_Kind::BUILTIN_TYPE, ident, 0);
    result->resolved_type = type;
    return result;
}

AST_Declaration* ast_variable_declaration(Instance* inst, AST_Identifier* ident, AST_Type_Spec* ts, AST_Expression* init)
{
    assert(ts || init);

    auto start_id = source_range_start(inst, ident->range_id);
    auto end_id = init ?
                    source_range_end(inst, init->range_id) :
                    source_range_end(inst, ts->range_id);
    auto range_id = source_range(inst, start_id, end_id);

    auto result = ast_declaration(inst, AST_Declaration_Kind::VARIABLE, ident, range_id);
    result->variable.type_spec = ts;
    result->variable.init_expr = init;
    result->variable.index = -1;
    return result;
}

AST_Declaration* ast_struct_member_declaration(Instance* inst, AST_Identifier* ident, AST_Type_Spec* ts, AST_Expression* default_val, u32 range_id)
{
    auto result = ast_declaration(inst, AST_Declaration_Kind::STRUCT_MEMBER, ident, range_id);
    result->variable.type_spec = ts;
    result->variable.init_expr = default_val;
    result->variable.index = -1;
    return result;
}

AST_Declaration* ast_struct_declaration(Instance* inst, AST_Identifier* ident, DArray<AST_Declaration*> fields, Scope* scope, u32 range_id)
{
    auto result = ast_declaration(inst, AST_Declaration_Kind::STRUCT, ident, range_id);
    result->structure.scope = scope;
    result->structure.fields = fields;
    return result;
}

AST_Declaration* ast_function_declaration(Instance* inst, AST_Identifier* ident, DArray<AST_Declaration*> param_decls, DArray<AST_Statement*> body_stmts, AST_Type_Spec* return_ts, Scope* scope, u32 range_id, u32 body_start_id)
{
    auto result = ast_declaration(inst, AST_Declaration_Kind::FUNCTION, ident, range_id);
    result->function.params = param_decls;
    result->function.body = body_stmts;
    result->function.return_ts = return_ts;
    result->function.scope = scope;
    darray_init(&inst->ast_allocator, &result->function.variables, 0);
    darray_init(&inst->ast_allocator, &result->function.temp_structs, 0);
    darray_init(&inst->ast_allocator, &result->function.wait_for_bytecode, 0);

    if (param_decls.count) {
        auto start = source_range_start(inst, param_decls[0]->range_id);
        auto end = source_range_end(inst, param_decls[param_decls.count - 1]->range_id);
        result->function.param_range_id = source_range(inst, start, end);
    } else {
        result->function.param_range_id = 0;
    }

    if (body_stmts.count) {
        auto end = source_range_end(inst, range_id);
        result->function.body_range_id = source_range(inst, body_start_id, end);
    } else {
        result->function.body_range_id = 0;
    }

    return result;
}

AST_Statement* ast_statement(Instance* inst, AST_Statement_Kind kind, u32 range_id)
{
    auto result = allocate<AST_Statement>(&inst->ast_allocator);
    result->kind = kind;
    result->flags = AST_STMT_FLAG_NONE;
    result->range_id = range_id;
    return result;
}

AST_Statement* ast_import_statement(Instance* inst, String_Ref path, u32 range_id)
{
    auto result = ast_statement(inst, AST_Statement_Kind::IMPORT, range_id);
    result->import_path = path;
    return result;
}

AST_Statement* ast_declaration_statement(Instance* inst, AST_Declaration* decl)
{
    auto result = ast_statement(inst, AST_Statement_Kind::DECLARATION, decl->range_id);
    result->declaration = decl;
    return result;
}

AST_Statement* ast_assignment_statement(Instance* inst, AST_Expression* lvalue, AST_Expression* rvalue)
{
    auto start_id = source_range_start(inst, lvalue->range_id);
    auto end_id = source_range_end(inst, rvalue->range_id);
    auto range_id = source_range(inst, start_id, end_id);

    auto result = ast_statement(inst, AST_Statement_Kind::ASSIGNMENT, range_id);
    result->assignment.lvalue = lvalue;
    result->assignment.rvalue = rvalue;
    return result;
}

AST_Statement* ast_arithmetic_assignment_statement(Instance* inst, u32 op, AST_Expression* lvalue, AST_Expression* rvalue)
{
    auto start_id = source_range_start(inst, lvalue->range_id);
    auto end_id = source_range_end(inst, rvalue->range_id);
    auto range_id = source_range(inst, start_id, end_id);

    auto result = ast_statement(inst, AST_Statement_Kind::ARITHMETIC_ASSIGNMENT, range_id);
    result->arithmetic_assignment.op = op;
    result->arithmetic_assignment.lvalue = lvalue;
    result->arithmetic_assignment.rvalue = rvalue;
    return result;
}

AST_Statement* ast_call_expr_statement(Instance* inst, AST_Expression* call)
{
    assert(call->kind == AST_Expression_Kind::CALL);

    auto result = ast_statement(inst, AST_Statement_Kind::CALL, call->range_id);
    result->call = call;
    return result;
}

AST_Statement* ast_return_statement(Instance* inst, AST_Expression* expr, u32 start_id)
{
    auto end_id = expr ? source_range_end(inst, expr->range_id) : 0;
    auto range_id = source_range(inst, start_id, end_id);

    auto result = ast_statement(inst, AST_Statement_Kind::RETURN, range_id);
    result->return_expr = expr;
    return result;
}

AST_Statement* ast_if_statement(Instance* inst, DArray<AST_If_Block> if_blocks, AST_Statement* else_stmt, u32 start_id)
{
    assert(if_blocks.count);

    u32 end_id = else_stmt ?
                    source_range_end(inst, else_stmt->range_id) :
                    source_range_end(inst, if_blocks[if_blocks.count - 1].then->range_id);

    auto range_id = source_range(inst, start_id, end_id);

    auto result = ast_statement(inst, AST_Statement_Kind::IF, range_id);
    result->if_stmt.blocks = if_blocks;
    result->if_stmt.else_stmt = else_stmt;
    return result;
}

AST_Statement* ast_while_statement(Instance* inst, AST_Expression* cond, AST_Statement* stmt, u32 start_id)
{
    auto end_id = source_range_end(inst, stmt->range_id);
    auto range_id = source_range(inst, start_id, end_id);

    auto result = ast_statement(inst, AST_Statement_Kind::WHILE, range_id);
    result->while_stmt.cond = cond;
    result->while_stmt.stmt = stmt;
    return result;
}

AST_Statement* ast_for_statement(Instance* inst, AST_Statement* init, AST_Expression* cond, AST_Statement* step, AST_Statement* stmt, Scope* scope, u32 start_id)
{
    auto end_id = source_range_end(inst, stmt->range_id);
    auto range_id = source_range(inst, start_id, end_id);

    auto result = ast_statement(inst, AST_Statement_Kind::FOR, range_id);
    result->for_stmt.init = init;
    result->for_stmt.cond = cond;
    result->for_stmt.step = step;
    result->for_stmt.stmt = stmt;
    result->for_stmt.scope = scope;
    return result;
}

AST_Statement* ast_break_statement(Instance* inst, u32 range_id)
{
    auto result = ast_statement(inst, AST_Statement_Kind::BREAK, range_id);
    result->loop_control_target = nullptr;
    return result;
}

AST_Statement* ast_continue_statement(Instance* inst, u32 range_id)
{
    auto result = ast_statement(inst, AST_Statement_Kind::CONTINUE, range_id);
    result->loop_control_target = nullptr;
    return result;
}

AST_Statement* ast_block_statement(Instance* inst, DArray<AST_Statement*> stmts, Scope* scope, u32 range_id)
{
    auto result = ast_statement(inst, AST_Statement_Kind::BLOCK, range_id);
    result->block.statements = stmts;
    result->block.scope = scope;
    return result;
}

AST_Expression* ast_expression(Instance* inst, AST_Expression_Kind kind, u32 range_id, AST_Expression_Flags flags/*=AST_EXPR_FLAG_NONE*/)
{
    auto result = allocate<AST_Expression>(&inst->ast_allocator);
    result->kind = kind;
    result->flags = flags;
    result->resolved_type = nullptr;
    result->range_id = range_id;
    return result;
}

AST_Expression* ast_identifier_expression(Instance* inst, AST_Identifier* ident)
{
    auto result = ast_expression(inst, AST_Expression_Kind::IDENTIFIER, ident->range_id);
    result->identifier = ident;
    return result;
}

NAPI AST_Expression* ast_binary_expression(Instance* inst, u32 op, AST_Expression* lhs, AST_Expression* rhs)
{
    auto start_id = source_range_start(inst, lhs->range_id);
    auto end_id = source_range_end(inst, rhs->range_id);
    auto range_id = source_range(inst, start_id, end_id);

    auto result = ast_expression(inst, AST_Expression_Kind::BINARY, range_id);
    result->binary.op = op;
    result->binary.lhs = lhs;
    result->binary.rhs = rhs;
    return result;
}

AST_Expression* ast_member_expression(Instance* inst, AST_Expression* base, AST_Identifier* member_name)
{
    auto start_id = source_range_start(inst, base->range_id);
    auto end_id = source_range_end(inst, member_name->range_id);
    auto range_id = source_range(inst, start_id, end_id);

    auto result = ast_expression(inst, AST_Expression_Kind::MEMBER, range_id);
    result->member.base = base;
    result->member.member_name = member_name;
    return result;
}

AST_Expression* ast_call_expression(Instance* inst, AST_Expression* base_expr, DArray<AST_Expression*> args, u32 range_id)
{
    auto result = ast_expression(inst, AST_Expression_Kind::CALL, range_id);
    result->call.base = base_expr;
    result->call.args = args;
    return result;
}

AST_Expression *ast_address_of_expression(Instance *instance, AST_Expression *operand, u32 start_id)
{
    auto end_id = source_range_end(instance, operand->range_id);
    auto range_id = source_range(instance, start_id, end_id);

    auto result = ast_expression(instance, AST_Expression_Kind::ADDRESS_OF, range_id);
    result->operand = operand;
    return result;
}

AST_Expression *ast_deref_expression(Instance *instance, AST_Expression *operand, u32 start_id)
{
    auto end_id = source_range_end(instance, operand->range_id);
    auto range_id = source_range(instance, start_id, end_id);

    auto result = ast_expression(instance, AST_Expression_Kind::DEREF, range_id);
    result->operand = operand;
    return result;
}

AST_Expression* ast_compound_expression(Instance* inst, DArray<AST_Expression*> expressions, u32 range_id)
{
    auto result = ast_expression(inst, AST_Expression_Kind::COMPOUND, range_id);
    result->compound.expressions = expressions;
    return result;
}

AST_Expression* ast_integer_literal_expression(Instance* inst, u64 i, u32 range_id)
{
    auto result = ast_expression(inst, AST_Expression_Kind::INTEGER_LITERAL, range_id, AST_EXPR_FLAG_CONST);
    result->integer_literal = i;
    return result;
}

AST_Expression* ast_real_literal_expression(Instance* inst, Real_Value rv, u32 range_id)
{
    auto result = ast_expression(inst, AST_Expression_Kind::REAL_LITERAL, range_id, AST_EXPR_FLAG_CONST);
    result->real_literal = rv;
    return result;
}

AST_Expression* ast_char_literal_expression(Instance* inst, char c, u32 range_id)
{
    auto result = ast_expression(inst, AST_Expression_Kind::CHAR_LITERAL, range_id, AST_EXPR_FLAG_CONST);
    result->char_literal = c;
    return result;
}

AST_Expression* ast_bool_literal_expression(Instance* inst, bool b, u32 range_id)
{
    auto result = ast_expression(inst, AST_Expression_Kind::BOOL_LITERAL, range_id, AST_EXPR_FLAG_CONST);
    result->bool_literal = b;
    return result;
}

AST_Expression* ast_string_literal_expression(Instance* inst, Atom atom, u32 range_id)
{
    auto result = ast_expression(inst, AST_Expression_Kind::STRING_LITERAL, range_id, AST_EXPR_FLAG_CONST);
    result->string_literal = atom;
    return result;
}

AST_Type_Spec* ast_type_spec(Instance* inst, AST_Type_Spec_Kind kind, u32 range_id)
{
    auto result = allocate<AST_Type_Spec>(&inst->ast_allocator);
    result->kind = kind;
    result->flags = AST_TS_FLAG_NONE;
    result->resolved_type = nullptr;
    result->range_id = range_id;
    return result;
}

AST_Type_Spec* ast_identifier_type_spec(Instance* inst, AST_Identifier* ident)
{
    auto result = ast_type_spec(inst, AST_Type_Spec_Kind::IDENTIFIER, ident->range_id);
    result->identifier = ident;
    return result;
}

AST_Type_Spec* ast_pointer_type_spec(Instance* inst, AST_Type_Spec *base, u32 start_id)
{
    auto end_id = source_range_end(inst, base->range_id);
    auto range_id = source_range(inst, start_id, end_id);

    auto result = ast_type_spec(inst, AST_Type_Spec_Kind::POINTER, range_id);
    result->base = base;
    return result;
}

AST_Identifier* ast_identifier(Instance* inst, Atom atom, u32 range_id)
{
    auto result = allocate<AST_Identifier>(&inst->ast_allocator);
    result->atom = atom;
    result->range_id = range_id;
    result->decl = nullptr;
    return result;
}

}
