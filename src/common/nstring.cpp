#include "nstring.h"

#include <cassert>
#include <cstring>

namespace Novo {

String_Ref::String_Ref() : data(nullptr), length(0) {}
String_Ref::String_Ref(const char *cstr) : data(cstr), length(strlen(cstr)) {}
String_Ref::String_Ref(const char *cstr, s64 length) : data(cstr), length(length) {}
String_Ref::String_Ref(const String &str) : data(str.data), length(str.length) {}

char String_Ref::operator[](s64 index) const
{
    assert(index >= 0 && index < this->length);
    return data[index];
}

String string(char *data, s64 length)
{
    return String { data, length };
}

String string_copy(Allocator allocator, const char *a_buf, s64 a_length)
{
    String result = {
        .data = allocate_array<char>(allocator, a_length + 1),
        .length = a_length,
    };

    memcpy(result.data, a_buf, a_length);
    result.data[a_length] = '\0';

    return result;
}

String string_append_internal(Allocator allocator, const char *a_buf, s64 a_length, const char *b_buf, s64 b_length)
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

}
