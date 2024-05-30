#include "const_resolver.h"

#include "ast.h"
#include "ssa.h"
#include "type.h"

namespace Novo {

Resolved_Constant const_resolve(Instance* inst, AST_Expression* expr)
{
    assert(expr->flags & AST_EXPR_FLAG_CONST);

    assert(expr->resolved_type);
    Type* result_type = expr->resolved_type;
    assert(result_type->kind == Type_Kind::INTEGER);

    Resolved_Constant result;
    result.status = Resolved_Constant_Status::UNRESOLVED;
    result.type = result_type;

    switch (expr->kind) {

        case AST_Expression_Kind::INVALID: assert(false); break;

        case AST_Expression_Kind::IDENTIFIER: {
            result = const_resolve_identifier(inst, expr->identifier);
            assert(result.type == result_type);
            break;
        }

        case AST_Expression_Kind::UNARY: {
            result = const_resolve(inst, expr->unary.operand);
            if (result.status != Resolved_Constant_Status::RESOLVED) return result;
            assert(result.type == result_type);

            switch (expr->unary.op) {
                default: assert(false);

                case '-': {
                    // TODO: FIXME: This should work for all integer sizes
                    result.integer = -(s64)result.integer;
                    break;
                }
            }

            break;
        }

        case AST_Expression_Kind::BINARY: assert(false); break;
        case AST_Expression_Kind::MEMBER: assert(false); break;
        case AST_Expression_Kind::IMPLICIT_MEMBER: assert(false); break;
        case AST_Expression_Kind::CALL: assert(false); break;
        case AST_Expression_Kind::SUBSCRIPT: assert(false); break;
        case AST_Expression_Kind::ADDRESS_OF: assert(false); break;
        case AST_Expression_Kind::DEREF: assert(false); break;
        case AST_Expression_Kind::CAST: assert(false); break;
        case AST_Expression_Kind::COMPOUND: assert(false); break;
        case AST_Expression_Kind::RUN: assert(false); break;
        case AST_Expression_Kind::SIZEOF: assert(false); break;
        case AST_Expression_Kind::ALIGNOF: assert(false); break;
        case AST_Expression_Kind::OFFSETOF: assert(false); break;
        case AST_Expression_Kind::TYPE: assert(false); break;

        case AST_Expression_Kind::INTEGER_LITERAL: {
            result.status = Resolved_Constant_Status::RESOLVED;
            result.integer = expr->integer_literal;
            break;
        }

        case AST_Expression_Kind::REAL_LITERAL: assert(false); break;
        case AST_Expression_Kind::CHAR_LITERAL: assert(false); break;
        case AST_Expression_Kind::BOOL_LITERAL: assert(false); break;
        case AST_Expression_Kind::NULL_LITERAL: assert(false); break;
        case AST_Expression_Kind::STRING_LITERAL: assert(false); break;

    }

    return result;
}

Resolved_Constant const_resolve_identifier(Instance* inst, AST_Identifier* ident)
{
    assert(ident->decl);
    AST_Declaration* decl = ident->decl;

    switch (decl->kind) {
        case AST_Declaration_Kind::INVALID: assert(false); break;
        case AST_Declaration_Kind::VARIABLE: assert(false); break;
        case AST_Declaration_Kind::CONSTANT: assert(false); break;
        case AST_Declaration_Kind::STRUCT_MEMBER: assert(false); break;
        case AST_Declaration_Kind::STRUCT: assert(false); break;

        case AST_Declaration_Kind::ENUM_MEMBER: {
            if (decl->enum_member.value_expr) {
                return const_resolve(inst, decl->enum_member.value_expr);
            } else {
                return { Resolved_Constant_Status::UNRESOLVED, decl->resolved_type };
            }
        }

        case AST_Declaration_Kind::ENUM: assert(false); break;
        case AST_Declaration_Kind::FUNCTION: assert(false); break;
        case AST_Declaration_Kind::BUILTIN_TYPE: assert(false); break;
    }

    assert(false);
    return { Resolved_Constant_Status::UNRESOLVED };
}

}
