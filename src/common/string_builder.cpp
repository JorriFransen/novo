#include "string_builder.h"

#include "memory/linear_allocator.h"
#include "memory/temp_allocator.h"

#include <cstring>

#define NOVO_SB_TEMP_SIZE 2048

namespace Novo {

struct String_Builder_Block
{
    char *cursor;
    const char *end;

    String_Builder_Block *next_block;
};


static String_Builder_Block *string_builder_block(Allocator *allocator, u64 size);

void string_builder_init(String_Builder *sb, Allocator *allocator, u64 initial_block_size/*=NOVO_SB_INITAL_BLOCK_SIZE*/)
{
    sb->allocator = allocator;
    auto ta = allocate<Temp_Allocator>(allocator);
    sb->temp_allocator = temp_allocator_create(ta, allocator, NOVO_SB_TEMP_SIZE);
    sb->next_block_size = initial_block_size;

    sb->first_block = string_builder_block(sb->allocator, sb->next_block_size);
    sb->current_block = sb->first_block;
}

void string_builder_free(String_Builder *sb)
{
    auto ta = (Temp_Allocator *)sb->temp_allocator.user_data;

    if (!(sb->temp_allocator.flags & ALLOCATOR_FLAG_CANT_FREE)) {
        free(sb->allocator, ta->linear_allocator_data.buffer);
        free(sb->allocator, ta);

        auto block = sb->first_block;
        while (block) {
            auto next = block->next_block;
            free(sb->allocator, block);
            block = next;
        }
    }
}

void string_builder_append(String_Builder *sb, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    string_builder_append_va(sb, fmt, args);

    va_end(args);
}

void string_builder_append_va(String_Builder *sb, const String_Ref fmt, va_list args)
{
    auto ta = (Temp_Allocator *)sb->temp_allocator.user_data;
    auto mark = temp_allocator_get_mark(ta);

    String temp_result = string_format_va_list(&sb->temp_allocator, fmt, args);

    auto remaining = sb->current_block->end - sb->current_block->cursor;

    if (remaining >= temp_result.length) {
        memcpy(sb->current_block->cursor, temp_result.data, temp_result.length);
        sb->current_block->cursor += temp_result.length;
    } else {

        auto size_a = remaining;
        auto size_b = temp_result.length - remaining;

        memcpy(sb->current_block->cursor, temp_result.data, size_a);
        sb->current_block->cursor += size_a;

        while (sb->next_block_size < size_b) sb->next_block_size *= 2;

        auto new_block = string_builder_block(sb->allocator, sb->next_block_size);
        sb->current_block->next_block = new_block;
        sb->current_block = new_block;

        memcpy(sb->current_block->cursor, temp_result.data + size_a, size_b);
        sb->current_block->cursor += size_b;
    }

    temp_allocator_reset(ta, mark);
}

static String_Builder_Block *string_builder_block(Allocator *allocator, u64 size)
{
    u64 alloc_size = sizeof(String_Builder_Block) + size;

    auto result = (String_Builder_Block *)allocate(allocator, alloc_size);
    result->cursor = (char *)&result[1];
    result->end = &result->cursor[size];
    result->next_block = nullptr;

    return result;
}

String string_builder_to_string(String_Builder *sb, Allocator *allocator)
{
    s64 size = 0;

    auto block = sb->first_block;
    while (block) {
        auto next = block->next_block;

        auto start = (char *)&block[1];
        auto block_size = block->cursor - start;
        size += block_size;

        block = next;
    }

    String result = {
        allocate_array<char>(allocator, size + 1),
        size,
    };

    auto cursor = result.data;

    block = sb->first_block;
    while (block) {
        auto next = block->next_block;

        auto start = (char *)&block[1];
        auto block_size = block->cursor - start;

        memcpy(cursor, start, block_size);
        cursor += block_size;

        block = next;
    }

    result.data[size] = '\0';

    return result;
}

String string_builder_to_string(String_Builder *sb)
{
    return string_builder_to_string(sb, sb->allocator);
}

}
