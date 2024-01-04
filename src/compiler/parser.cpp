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

    auto mark = temp_allocator_get_mark(&instance->temp_allocator_data);

    bool read_ok = fs_read_entire_file(&instance->temp_allocator, file_path, &file_content);
    assert(read_ok);

    lexer_init_stream(&lexer, file_content, file_path);

    while (!is_token(&lexer, TOK_EOF) && !lexer.error) {
        auto pos = instance->source_positions[lexer.token.source_pos_id];
        printf("%llu:%llu:%llu: ", pos.line, pos.index_in_line, pos.length);
        printf("'%s' :\t'%s'\n", tmp_token_kind_str(lexer.token.kind).data, tmp_token_str(&instance->atoms, lexer.token).data);
        next_token(&lexer);
    }

    temp_allocator_reset(&instance->temp_allocator_data, mark);
}

}
