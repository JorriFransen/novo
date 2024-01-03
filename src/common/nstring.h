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

    String_Ref();
    String_Ref(const char *cstr);
    String_Ref(const char *cstr, s64 length);
};

NAPI String string(char *data, s64 length);

String string_append_internal(Allocator allocator, const char *a_buf, s64 a_length, const char *b_buf, s64 b_length);
String string_append_internal(Allocator allocator, const char *a_buf, s64 a_length, const char *b_buf, s64 b_length);
String string_copy_internal(Allocator allocator, const char *a_buf, s64 a_length);

NINLINE String string_append(Allocator allocator, const String a, const String b)
{
    return string_append_internal(allocator, a.data, a.length, b.data, b.length);
}

NINLINE String string_append(Allocator allocator, const String_Ref a, const String_Ref b)
{
    return string_append_internal(allocator, a.data, a.length, b.data, b.length);
}

}
