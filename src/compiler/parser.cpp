#include "parser.h"

#include "lexer.h"
#include "platform.h"

#include <cassert>
#include <cstdio>

namespace Novo {

void parse_file(Instance *instance, const String_Ref file_path)
{
    Lexer lexer;
    lexer_create(instance, &lexer);

    String file_content;

    // TODO: use temp allocator
    bool read_ok = fs_read_entire_file(c_allocator(), file_path, &file_content);
    assert(read_ok);

    lexer_init_stream(&lexer, file_content, file_path);

    while (!is_token(&lexer, TOK_EOF) && !lexer.error) {
        auto pos = instance->source_positions[lexer.token.source_pos_id];
        printf("%llu:%llu: error:\n", pos.line, pos.index_in_line);
        printf("'%s' :\t'%s'\n", tmp_token_kind_str(lexer.token.kind).data, tmp_token_str(lexer.token).data);
        next_token(&lexer);
    }
}

}
