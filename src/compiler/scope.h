#pragma once

#include <containers/darray.h>
#include <defines.h>

#include "atom.h"

namespace Novo {

struct AST_Declaration;
struct Instance;

struct Symbol
{
    Atom atom;
    AST_Declaration *decl;
};

enum Scope_Kind : u32
{
    INVALID,
    GLOBAL,
    STRUCT,
    FUNCTION_LOCAL,
};

struct Scope
{
    Scope_Kind kind;

    Scope *parent;

    DArray<Symbol> symbols;
};

NAPI Scope *scope_new(Instance *instance, Scope_Kind kind, Scope *parent = nullptr);
NAPI bool scope_add_symbol(Scope *scope, Atom atom, AST_Declaration *decl);
NAPI AST_Declaration *scope_find_symbol(Scope *scope, Atom atom, Scope **found_in_scope);

NAPI bool scope_is_parent(Scope *child, Scope *parent);

}
