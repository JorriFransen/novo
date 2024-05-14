#include "scope.h"

#include <memory/allocator.h>

#include "atom.h"
#include "instance.h"

namespace Novo {

Scope* scope_new(Instance* instance, Scope_Kind kind, Scope* parent/*=nullptr*/)
{
    Scope* result = allocate<Scope>(&instance->scope_allocator);
    result->kind = kind;
    result->parent = parent;

    darray_init(&instance->scope_allocator, &result->symbols, 0);

    return result;
}

bool scope_add_symbol(Scope* scope, Atom atom, AST_Declaration* decl, Scope_Find_Options opts/*=SCOPE_FIND_OPTS_NONE*/)
{
    if (!(opts & SCOPE_FIND_OPTS_NO_REDECL_CHECK)) {
        if (scope_find_symbol(scope, atom, nullptr, opts)) {
            return false;
        }
    }

    darray_append(&scope->symbols, { atom, decl });
    return true;
}

AST_Declaration* scope_find_symbol(Scope* scope, Atom atom, Scope **found_in_scope, Scope_Find_Options opts/*=SCOPE_FIND_OPTS_NONE*/)
{
    if ((opts & SCOPE_FIND_OPTS_LIMIT_TO_TYPE_DECL) &&
        (scope->kind != Scope_Kind::STRUCT && scope->kind != Scope_Kind::ENUM)) {
        return nullptr;
    }

    for (s64 i = 0; i < scope->symbols.count; i++) {
        if (scope->symbols[i].atom == atom) {

            if (found_in_scope) *found_in_scope = scope;
            return scope->symbols[i].decl;
        }
    }

    if (scope->parent) {
        return scope_find_symbol(scope->parent, atom, found_in_scope, opts);
    }

    return nullptr;
}

bool scope_is_parent(Scope* child, Scope* parent)
{
    if (!child->parent) return false;
    if (child->parent == parent) return true;
    return scope_is_parent(child->parent, parent);

}

}
