#include "scope.h"

#include <memory/allocator.h>

#include "atom.h"
#include "instance.h"

namespace Novo {

Scope *scope_new(Instance *instance, Scope_Kind kind, Scope *parent/*=nullptr*/)
{
    Scope *result = allocate<Scope>(&instance->scope_allocator);
    result->kind = kind;
    result->parent = parent;

    darray_init(&instance->scope_allocator, &result->symbols, 0);

    return result;
}

bool scope_add_symbol(Scope *scope, Atom atom, AST_Declaration *decl)
{
    if (scope_find_symbol(scope, atom, nullptr)) {
        return false;
    }

    darray_append(&scope->symbols, { atom, decl });
    return true;
}

AST_Declaration *scope_find_symbol(Scope *scope, Atom atom, Scope **found_in_scope)
{
    for (s64 i = 0; i < scope->symbols.count; i++) {
        if (scope->symbols[i].atom == atom) {

            if (found_in_scope) *found_in_scope = scope;
            return scope->symbols[i].decl;
        }
    }

    if (scope->parent) {
        return scope_find_symbol(scope->parent, atom, found_in_scope);
    }

    return nullptr;
}

bool scope_is_parent(Scope *child, Scope *parent)
{
    if (!child->parent) return false;
    if (child->parent == parent) return true;
    return scope_is_parent(child->parent, parent);

}

}
