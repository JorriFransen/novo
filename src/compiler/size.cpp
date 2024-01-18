#include "size.h"

#include "ast.h"
#include "scope.h"

namespace Novo {

bool size_declaration(Instance *inst, Task *task, AST_Declaration *decl, Scope *scope)
{
    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false); break;
        case AST_Declaration_Kind::VARIABLE: assert(false); break;

        case AST_Declaration_Kind::FUNCTION: {

            assert(scope != decl->function.scope);
            auto fn_scope = decl->function.scope;

            for (s64 i = 0; i < decl->function.params.count; i++) {
                if (!size_declaration(inst, task, decl->function.params[i], fn_scope)) {
                    return false;
                }
            }

            if (decl->function.return_ts) {
                if (!size_type_spec(inst, task, decl->function.return_ts, fn_scope)) {
                    return false;
                }
            }

            assert(false);
            break;
        }
    }

    assert(false);
}

bool size_type_spec(Instance *inst, Task *task, AST_Type_Spec *ts, Scope *scope)
{
    switch (ts->kind) {

        case AST_Type_Spec_Kind::INVALID: assert(false); break;

        case AST_Type_Spec_Kind::IDENTIFIER: {
            return size_identifier(inst, task, ts->identifier, scope); 
        };
    }

    assert(false);
}

NAPI bool size_identifier(Instance *inst, Task *task, AST_Identifier *ident, Scope *scope)
{
    assert(ident->decl);
    assert(false);
}

}
