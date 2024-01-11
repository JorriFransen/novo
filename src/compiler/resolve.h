#pragma once

#include <defines.h>

namespace Novo {

struct AST_Declaration;
struct Scope;
struct Instance;

NAPI bool resolve_declaration(Instance *instance, AST_Declaration *decl, Scope *scope);

}
