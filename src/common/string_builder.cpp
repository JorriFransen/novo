#include "string_builder.h"

#include "memory/allocator.h"
#include "memory/arena.h"

#include <cstring>

#define NOVO_SB_TEMP_SIZE 2048

namespace Novo {

struct String_Builder_Block
{
    char* cursor;
    const char* end;

    String_Builder_Block* next_block;
};


static String_Builder_Block* string_builder_block(Allocator* allocator, u64 size);

void string_builder_init(String_Builder* sb, Allocator* allocator, u64 initial_block_size/*=NOVO_SB_INITAL_BLOCK_SIZE*/)
{
    sb->allocator = allocator;
    sb->next_block_size = initial_block_size;

    sb->first_block = string_builder_block(sb->allocator, sb->next_block_size);
    sb->current_block = sb->first_block;
}

void string_builder_free(String_Builder* sb)
{
    auto block = sb->first_block;
    while (block) {
        auto next = block->next_block;
        nrelease(sb->allocator, block);
        block = next;
    }
}

void string_builder_reset(String_Builder* sb)
{
    sb->current_block = sb->first_block;

    auto block = sb->first_block;

    while (block) {
        auto next = block->next_block;
        block->cursor = (char*)&block[1];
        block = next;
    }
}

void string_builder_append(String_Builder* sb, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    string_builder_append_va(sb, fmt, args);

    va_end(args);
}

void string_builder_append_va(String_Builder* sb, const String_Ref fmt, va_list args)
{
    Temp_Arena tarena = temp_arena((Arena*)sb->allocator->user_data);
    Allocator ta = arena_allocator_create(tarena.arena);

    String temp_result = string_format_va_list(&ta, fmt, args);

    auto remaining = sb->current_block->end - sb->current_block->cursor;

    if (remaining >= temp_result.length) {
        memcpy(sb->current_block->cursor, temp_result.data, temp_result.length);
        sb->current_block->cursor += temp_result.length;
    } else {

        auto size_a = remaining;
        auto size_b = temp_result.length - remaining;

        memcpy(sb->current_block->cursor, temp_result.data, size_a);
        sb->current_block->cursor += size_a;

        auto read_cursor = temp_result.data + size_a;

        while (size_b && sb->current_block->next_block) {

            sb->current_block = sb->current_block->next_block;

            auto write_size = min(size_b, (s64)(sb->current_block->end - sb->current_block->cursor));
            memcpy(sb->current_block->cursor, read_cursor, write_size);
            sb->current_block->cursor += write_size;
            read_cursor += write_size;
            size_b -= write_size;
        }

        if (size_b) {
            while (sb->next_block_size < size_b) sb->next_block_size *= 2;

            auto new_block = string_builder_block(sb->allocator, sb->next_block_size);
            sb->current_block->next_block = new_block;
            sb->current_block = new_block;

            memcpy(sb->current_block->cursor, temp_result.data + size_a, size_b);
            sb->current_block->cursor += size_b;
        }
    }

    temp_arena_release(tarena);
}

static String_Builder_Block* string_builder_block(Allocator* allocator, u64 size)
{
    u64 alloc_size = sizeof(String_Builder_Block) + size;

    String_Builder_Block* result = nallocate_size(allocator, alloc_size, String_Builder_Block);
    result->cursor = (char*)&result[1];
    result->end = &result->cursor[size];
    result->next_block = nullptr;

    return result;
}

String string_builder_to_string(String_Builder* sb, Allocator* allocator)
{
    s64 size = 0;

    auto block = sb->first_block;
    while (block) {
        auto next = block->next_block;

        auto start = (char*)&block[1];
        auto block_size = block->cursor - start;
        size += block_size;

        block = next;
    }

    if (size <= 0) return {};

    String result = {
        nallocate_array(allocator, char, size + 1),
        size,
    };

    auto cursor = result.data;

    block = sb->first_block;
    while (block) {
        auto next = block->next_block;

        auto start = (char*)&block[1];
        auto block_size = block->cursor - start;

        memcpy(cursor, start, block_size);
        cursor += block_size;

        block = next;
    }

    result.data[size] = '\0';

    return result;
}

String string_builder_to_string(String_Builder* sb)
{
    return string_builder_to_string(sb, sb->allocator);
}

}
