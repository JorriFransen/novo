#include "nstring.h"

#include "memory/allocator.h"

#include <cassert>
#include <cstdio>
#include <cstring>

#define ZSTRING_FORMAT_STACK_BUFFER_SIZE 32000

namespace Novo {

char &String::operator[](s64 index)
{
    assert(index >= 0 && index < this->length);
    return data[index];
}

String_Ref::String_Ref() : data(nullptr), length(0) {}
String_Ref::String_Ref(const char *cstr) : data(cstr), length(strlen(cstr)) {}
String_Ref::String_Ref(const char *cstr, s64 length) : data(cstr), length(length) {}
String_Ref::String_Ref(const String &str) : data(str.data), length(str.length) {}

const char &String_Ref::operator[](s64 index) const
{
    assert(index >= 0 && index < this->length);
    return data[index];
}

String string(char *data, s64 length)
{
    return String { data, length };
}

bool string_equal(String &a, String &b)
{
    if (a.length != b.length) return false;
    if (a.data == b.data) return true;

    return memcmp(a.data, b.data, a.length) == 0;
}

bool string_equal(const String_Ref &a, const String_Ref &b)
{
    if (a.length != b.length) return false;
    if (a.data == b.data) return true;

    return memcmp(a.data, b.data, a.length) == 0;
}

bool string_ends_with(const String_Ref &str, const String_Ref &end)
{
    if (end.length > str.length) return false;

    String_Ref substr(str.data + (str.length - end.length), end.length);
    return string_equal(substr, end);
}

String string_copy(Allocator *allocator, const char *a_buf, s64 a_length)
{
    String result = {
        allocate_array<char>(allocator, a_length + 1),
        a_length,
    };

    memcpy(result.data, a_buf, a_length);
    result.data[a_length] = '\0';

    return result;
}

String string_append_internal(Allocator *allocator, const char *a_buf, s64 a_length, const char *b_buf, s64 b_length)
{
    if (a_length <= 0) {
        return string_copy(allocator, b_buf, b_length);
    } else if (b_length <= 0) {
        return string_copy(allocator, a_buf, a_length);
    }

    s64 new_length = a_length + b_length;
    auto new_buf = allocate_array<char>(allocator, new_length + 1);

    memcpy(new_buf, a_buf, a_length);
    memcpy(&new_buf[a_length], b_buf, b_length);
    new_buf[new_length] = '\0';

    return string(new_buf, new_length);

}

String string_format(Allocator *allocator, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    auto result = string_format_va_list(allocator, fmt, args);

    va_end(args);

    return result;
}

const String string_format_va_list(Allocator *allocator, const String_Ref fmt, va_list args)
{
    va_list args_copy;
    va_copy(args_copy, args);

    assert(fmt.data[fmt.length] == '\0' && "Null terminated string expected");
    auto size = vsnprintf(nullptr, 0, (const char *)fmt.data, args_copy);

    va_end(args_copy);

    char *buf = allocate_array<char>(allocator, size + 1);
    assert(buf);

    auto written_size = vsnprintf(buf, (size_t)size + 1, fmt.data, args);
    assert(written_size <= size);

    assert(written_size <= size && "Written size does not match the expected size");

    return string(buf, size);
}

s32 string_format(char *dest, const String_Ref fmt, ...)
{
    va_list args;
    va_start(args, fmt);

    auto out_length = string_format(dest, fmt, args);
    assert(out_length < ZSTRING_FORMAT_STACK_BUFFER_SIZE);

    va_end(args);

    return out_length;
}

s32 string_format(char *dest, const String_Ref fmt, va_list args)
{
    static char buffer[ZSTRING_FORMAT_STACK_BUFFER_SIZE];

    assert(fmt.data[fmt.length] == '\0' && "Null terminated string expected");
    auto written_size = vsnprintf(buffer, ZSTRING_FORMAT_STACK_BUFFER_SIZE, fmt.data, args);
    buffer[written_size] = '\0';
    memcpy(dest, buffer, written_size + 1);

    return written_size;
}

// TODO: Emit these arrays with a macro
static char special_characters[] = {
    '\n',
    '\t',
    '\"',
};

static char escape_characters[] = {
    'n',
    't',
    '"',
};

s64 is_special_character(char c)
{
    for (s64 i = 0; i < (s64)(sizeof(special_characters) / sizeof(special_characters[0])); i++) {
        if (c  == special_characters[i]) return i;
    }

    return -1;
}

s64 is_escape_character(char c)
{
    for (s64 i = 0; i < (s64)(sizeof(escape_characters) / sizeof(escape_characters[0])); i++) {
        if (c == escape_characters[i]) return i;
    }

    return -1;
}

String convert_special_characters_to_escape_characters(Allocator *allocator, const String_Ref str)
{
    if (str.length <= 0) {
        return { nullptr, 0 };
    }

    s64 special_count = 0;

    for (s64 i = 0; i < str.length; i++) {
        auto c = str[i];

        if (is_special_character(c) != -1) {
            special_count += 1;
            break;
        }
    }

    if (!special_count) {
        return string_copy(allocator, str);
    }

    auto new_length = str.length + special_count;

    auto data = allocate_array<char>(allocator, new_length + 1);

    s64 ni = 0;
    for (s64 i = 0; i < str.length; i++) {
        auto special_index = is_special_character(str[i]);
        if (special_index != -1) {
            data[ni++] = '\\';
            data[ni++] = escape_characters[special_index];
        } else {
            data[ni++] = str[i];
        }
    }

    data[new_length] = '\0';
    return string(data, new_length);
}

String convert_escape_characters_to_special_characters(Allocator *allocator, const String_Ref str, const char **err_char/*=nullptr*/)
{
    s64 escape_count = 0;

    for (s64 i = 0; i < str.length; i++) {
        auto c = str[i];

        if (c == '\\') {
            assert(i + 1 < str.length);

            if (is_escape_character(str[i + 1]) == -1) {
                if (err_char) {
                    *err_char = &str[i + 1];
                } else {
                    assert(false && "Invalid escape character!");
                }
            }

            escape_count += 1;
        }
    }

    if (!escape_count) {
        return string_copy(allocator, str);
    }

    auto new_length = str.length - escape_count;

    auto data = allocate_array<char>(allocator, new_length + 1);

    s64 ni = 0;
    for (s64 i = 0; i < str.length; i++) {
        if (str[i] == '\\')  {
            // debug_assert(i + 1 < str.length);
            assert(i + 1 < str.length);
            i += 1;
            auto escape_index = is_escape_character(str[i]);
            data[ni++] = special_characters[escape_index];
        } else {
            data[ni++] = str[i];
        }
    }

    data[new_length] = '\0';
    return string(data, new_length);
}


u64 hash_string(const char *cstr, u64 length)
{
    u64 hash = 14695981039346656037u;

    for (u64 i = 0; i < length; i++)
    {
        hash = hash ^ ((u64)cstr[i]);
        hash = hash * 1099511628211;
    }

    return hash;
}

u64 hash_string(const char *cstr)
{
    return hash_string(cstr, strlen(cstr));
}

}
