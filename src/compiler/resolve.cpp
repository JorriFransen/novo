#include "resolve.h"

#include "ast.h"

#include <cassert>

namespace Novo {

bool resolve_declaration(Instance *instance, AST_Declaration *decl, Scope *scope)
{
    switch (decl->kind) {

        case AST_Declaration_Kind::INVALID: assert(false); break;

        case AST_Declaration_Kind::VARIABLE: {
            assert(false);
            break;
        }

        case AST_Declaration_Kind::FUNCTION: {
            assert(false);
            break;
        }
    }

    assert(false);
    return false;
}

}
