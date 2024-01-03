#pragma once

#include "defines.h"
#include "memory/allocator.h"

namespace Novo {

struct String {
    char *data;
    s64 length;
};


struct String_Ref {
    const char *data;
    s64 length;

    NAPI String_Ref();
    NAPI String_Ref(const char *cstr);
    NAPI String_Ref(const char *cstr, s64 length);
    NAPI String_Ref(const String &str);

    NAPI char operator[](s64 index) const;
};

NAPI String string(char *data, s64 length);

String string_append_internal(Allocator allocator, const char *a_buf, s64 a_length, const char *b_buf, s64 b_length);
String string_append_internal(Allocator allocator, const char *a_buf, s64 a_length, const char *b_buf, s64 b_length);

NAPI String string_copy(Allocator allocator, const char *a_buf, s64 a_length);

NINLINE String string_copy(Allocator allocator, const String str)
{
    return string_copy(allocator, str.data, str.length);
}

NINLINE String string_copy(Allocator allocator, const String_Ref str)
{
    return string_copy(allocator, str.data, str.length);
}

NINLINE String string_append(Allocator allocator, const String a, const String b)
{
    return string_append_internal(allocator, a.data, a.length, b.data, b.length);
}

NINLINE String string_append(Allocator allocator, const String_Ref a, const String_Ref b)
{
    return string_append_internal(allocator, a.data, a.length, b.data, b.length);
}

#define NSTRING_ASSERT_ZERO_TERMINATION(str) assert((str).data[(str).length] == '\0')

}
