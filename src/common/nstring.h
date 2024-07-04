#pragma once

#include "defines.h"

#include <cassert>
#include <containers/darray.h>
#include <cstdarg>

#if NPLATFORM_WINDOWS
#include <windows.h>
#endif // NPLATFORM_WINDOWS

namespace Novo {

struct Allocator;

struct String {
    char* data;
    s64 length;

    NAPI char &operator[](s64 index);
};

struct String_Ref {
    const char* data;
    s64 length;

    NAPI String_Ref();
    NAPI String_Ref(const char* cstr);
    NAPI String_Ref(const char* cstr, s64 length);
    NAPI String_Ref(const String &str);

    NAPI const char &operator[](s64 index) const;
};

NAPI String string(char* data, s64 length);

NAPI bool string_equal(String &a, String &b);
NAPI bool string_equal(const String_Ref &a, const String_Ref &b);
NAPI bool string_equal(const char* a, const char* b);

NAPI bool string_contains(const String_Ref &str, char c);
NAPI s64 string_last_index_of(const String_Ref &str, char c);

NAPI bool string_starts_with(const String_Ref &str, const String_Ref &start);
NAPI bool string_ends_with(const String_Ref &str, const String_Ref &end);

NAPI String string_append_internal(Allocator* allocator, const char* a_buf, s64 a_length, const char* b_buf, s64 b_length);

#ifdef NOVO_TRACE_ALLOC

#   define string_copy(allocator, ...) \
        string_copy_impl((allocator), __VA_ARGS__, __FILE__, __LINE__)

    NAPI String string_copy_impl(Allocator* allocator, const char* a_buf, s64 a_length, const char* file, s64 line);

    NINLINE String string_copy_impl(Allocator* allocator, const String str, const char* file, s64 line)
    {
        return string_copy_impl(allocator, str.data, str.length, file, line);
    }

    NINLINE String string_copy_impl(Allocator* allocator, const String_Ref str, const char* file, s64 line)
    {
        return string_copy_impl(allocator, str.data, str.length, file, line);
    }

#else // NOVO_TRACE_ALLOC

    NAPI String string_copy(Allocator* allocator, const char* a_buf, s64 a_length);

    NINLINE String string_copy(Allocator* allocator, const String str)
    {
        return string_copy(allocator, str.data, str.length);
    }

    NINLINE String string_copy(Allocator* allocator, const String_Ref str)
    {
        return string_copy(allocator, str.data, str.length);
    }

#endif // NOVO_TRACE_ALLOC

NINLINE String string_append(Allocator* allocator, const String a, const String b)
{
    return string_append_internal(allocator, a.data, a.length, b.data, b.length);
}

NINLINE String string_append(Allocator* allocator, const String_Ref a, const String_Ref b)
{
    return string_append_internal(allocator, a.data, a.length, b.data, b.length);
}

NINLINE String string_append(Allocator* allocator, Array_Ref<String_Ref> strings, String_Ref separator = "")
{
    assert(strings.count >= 1);

    s64 length = 0;
    for (s64 i = 0; i < strings.count; i++) length += strings[i].length;
    length += separator.length * strings.count - 1;
    assert(length);

    String result;
    result.data = nallocate_array(allocator, char, length + 1);
    result.length = length;

    s64 offset = 0;
    for (s64 i = 0; i < strings.count; i++) {
        memcpy(result.data + offset, strings[i].data, strings[i].length);
        offset += strings[i].length;
        memcpy(result.data + offset, separator.data, separator.length);
        offset += separator.length;
    }

    result.data[length] = '\0';

    return result;
}

NAPI String string_format(Allocator* allocator, const String_Ref fmt, ...);
NAPI const String string_format_va_list(Allocator* allocator, const String_Ref fmt, va_list args);

NAPI s32 string_format(char* dest, const String_Ref fmt, ...);
NAPI s32 string_format(char* dest, const String_Ref fmt, va_list args);

NAPI s64 is_special_character(char c);
NAPI s64 is_escape_character(char c);

NAPI char get_special_char(s64 index);
NAPI char get_escape_char(s64 index);

NAPI String convert_special_characters_to_escape_characters(Allocator* allocator, const String_Ref str);
NAPI String convert_escape_characters_to_special_characters(Allocator* allocator, const String_Ref str, const char **err_char = nullptr);

NAPI s64 convert_string_to_signed(const String_Ref& str, u32 base = 10);

NAPI u64 hash_string(const char* cstr, u64 length);
NAPI u64 hash_string(const char* cstr);

#define NSTRING_ASSERT_ZERO_TERMINATION(str) assert((str).data[(str).length] == '\0')

#if NPLATFORM_WINDOWS

struct Wide_String
{
    wchar_t* data;
    s64 length;

    NAPI Wide_String(Allocator* allocator, const String_Ref ref);
    NAPI Wide_String(wchar_t* wstr);

    NAPI wchar_t &operator[](s64 index)
    {
        assert(index >= 0 && index < this->length);
        return data[index];
    }
};

NAPI String wide_string_to_regular(Allocator* allocator, const Wide_String wstring);

#endif //NPLATFORM_WINDOWS

}
