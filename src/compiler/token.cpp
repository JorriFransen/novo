#include "token.h"

#include <cctype>
#include <string.h>

namespace Novo {

bool is_binary_arithmetic_op(u32 op)
{
    return is_binary_arithmetic_op((Token_Kind)op);
}

bool is_binary_cmp_op(u32 op)
{
    return is_binary_cmp_op((Token_Kind)op);
}

bool is_binary_op(u32 op)
{
    return is_binary_op((Token_Kind)op);
}

bool is_binary_arithmetic_op(Token_Kind op)
{
    switch ((u32)op) {
        case '+':
        case '-':
        case '*':
        case '/':
            return true;
    }

    return false;
}

bool is_binary_cmp_op(Token_Kind op)
{
    switch ((u32)op) {
        case '<':
        case '>':
        case TOK_EQ:
        case TOK_NEQ:
        case TOK_LTEQ:
        case TOK_GTEQ:
            return true;
    }

    return false;
}

 bool is_binary_op(Token_Kind op)
{
    return is_binary_arithmetic_op(op) || is_binary_cmp_op(op);
}

String_Ref tmp_token_str(Token token)
{
    static char buffer[1024];
    s32 length = 0;

    if (token.atom != 0) {
        auto str = atom_string(token.atom);
        length = str.length;
        memcpy(buffer, str.data, length);
        buffer[length] = '\0';
    } else if (token.kind == TOK_EOF) {
        return token_kind_to_string(token.kind);
    } else {
        assert(false);
    }

    auto buf = buffer;
    if (length == 0) buf = nullptr;
    return String_Ref(buf, length);
}

String_Ref tmp_token_kind_str(Token_Kind kind)
{
    static char buffer[256];
    s32 length = 0;

    if (isprint((char)kind)) {
        length = string_format(buffer, "%c", (char)kind);
    } else {
        return token_kind_to_string(kind);
    }

    auto buf = buffer;
    if (length == 0) buf = nullptr;
    return String_Ref(buf, length);
}

}
