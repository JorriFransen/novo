#pragma once

#include <defines.h>
#include <nstring.h>

namespace Novo {

struct Allocator;
struct AST_File;
struct String_Builder;

NAPI String ast_to_string(AST_File *file, String_Builder *sb);
NAPI String ast_to_string(AST_File *file, Allocator *allocator);

}
