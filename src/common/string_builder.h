#pragma once

#include "defines.h"
#include "nstring.h"
#include "memory/allocator.h"

#include <cstdarg>

#define NOVO_SB_INITAL_BLOCK_SIZE 2048

namespace Novo {

struct String_Builder_Block;

struct String_Builder
{
    Allocator* allocator;

    s64 next_block_size;

    String_Builder_Block* first_block;
    String_Builder_Block* current_block;
};

NAPI void string_builder_init(String_Builder* sb, Allocator* allocator, u64 initial_block_size = NOVO_SB_INITAL_BLOCK_SIZE);
NAPI void string_builder_free(String_Builder* sb);

NAPI void string_builder_reset(String_Builder* sb);

NAPI void string_builder_append(String_Builder* sb, const String_Ref fmt, ...);
NAPI void string_builder_append_va(String_Builder* sb, const String_Ref fmt, va_list args);

NAPI String string_builder_to_string(String_Builder* sb, Allocator* allocator);
NAPI String string_builder_to_string(String_Builder* sb);

}
