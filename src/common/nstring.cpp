#include "nstring.h"

#include <cassert>
#include <cstring>

namespace Novo {

String_Ref::String_Ref() : data(nullptr), length(0) {}
String_Ref::String_Ref(const char *cstr) : data(cstr), length(strlen(cstr)) { }
String_Ref::String_Ref(const char *cstr, s64 length) : data(cstr), length(length) {}


String string(char *data, s64 length)
{
    return String { data, length };
}

String string_append_internal(Allocator allocator, const char *a_buf, s64 a_length, const char *b_buf, s64 b_length)
{
    if (a_length <= 0) {
        return string_copy_internal(allocator, b_buf, b_length);
    } else if (b_length <= 0) {
        return string_copy_internal(allocator, a_buf, a_length);
    }

    s64 new_length = a_length + b_length;
    auto new_buf = allocate_array<char>(allocator, new_length + 1);

    memcpy(new_buf, a_buf, a_length);
    memcpy(&new_buf[a_length], b_buf, b_length);
    new_buf[new_length] = '\0';

    return string(new_buf, new_length);

}

String string_copy_internal(Allocator allocator, const char *a_buf, s64 a_length)
{
    assert(false);
}

}
