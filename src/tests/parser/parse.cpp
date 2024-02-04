
#include <ast_print.h>
#include <defines.h>
#include <instance.h>
#include <memory/allocator.h>
#include <nstring.h>
#include <options.h>
#include <parser.h>

#include <cstdio>
#include <cassert>
#include <cstring>

using namespace Novo;

int main(int argc, char* argv[])
{
    Instance instance;
    instance_init(&instance, default_options());

    auto file = parse_file(&instance, "test.no");
    assert(file);

    auto ast_str = ast_to_string(&instance, file, c_allocator());

    auto expected_ast_str = R"(002:001-007:001  FUNC_DECL: 'main'
                  RETURN_TS:
002:015-0000003    IDENT_TS: 'int'
                  PARAMS: 0
002:019-007:001   BODY:
003:005-003:012    VAR_DECL: 'x'
003:010-0000003     EXPR_CHAR: 'a'
004:005-004:014    VAR_DECL: 'y'
004:010-0000005     EXPR_STR: "abc"
005:005-005:011    VAR_DECL: 'z'
005:010-0000002     EXPR_INT: 42
006:005-006:012    STMT_RETURN:
006:012-0000001     EXPR_INT: 0

009:001-011:001  FUNC_DECL: 'add'
                  RETURN_TS:
009:028-0000003    IDENT_TS: 'int'
009:009-009:022   PARAMS: 2
009:009-009:014    VAR_DECL: 'a'
009:012-0000003     IDENT_TS: 'int'
009:017-009:022    VAR_DECL: 'b'
009:020-0000003     IDENT_TS: 'int'
009:032-011:001   BODY:
010:005-010:016    STMT_RETURN:
010:012-010:016     EXPR_BINARY: '+'
010:012-0000001      EXPR_IDENT: 'a'
010:016-0000001      EXPR_IDENT: 'b'

013:001-020:001  FUNC_DECL: 'test'
                  RETURN_TS:
013:015-0000003    IDENT_TS: 'int'
                  PARAMS: 0
013:019-020:001   BODY:
014:005-014:011    VAR_DECL: 'x'
014:009-0000003     IDENT_TS: 'int'
015:005-015:011    VAR_DECL: 'y'
015:009-0000003     IDENT_TS: 'int'
016:005-016:011    VAR_DECL: 'z'
016:009-0000003     IDENT_TS: 'int'
018:005-018:022    VAR_DECL: 'r'
018:010-018:022     EXPR_BINARY: '+'
018:010-018:014      EXPR_BINARY: '*'
018:010-0000001       EXPR_IDENT: 'x'
018:014-0000001       EXPR_IDENT: 'y'
018:018-018:022      EXPR_BINARY: '/'
018:018-0000001       EXPR_IDENT: 'z'
018:022-0000001       EXPR_INT: 2
019:005-019:025    VAR_DECL: 'r2'
019:011-019:025     EXPR_BINARY: '/'
019:011-019:020      EXPR_BINARY: '*'
019:011-0000001       EXPR_IDENT: 'x'
019:016-019:020       EXPR_BINARY: '+'
019:016-0000001        EXPR_IDENT: 'y'
019:020-0000001        EXPR_IDENT: 'z'
019:025-0000001      EXPR_INT: 2

)";

    auto expected_length = (s64)strlen(expected_ast_str);
    fprintf(stderr, "ast_str.length: %lld\n", ast_str.length);
    fprintf(stderr, "expected_length: %lld\n", expected_length);
    fprintf(stderr, "ast_str: \"%s\"", ast_str.data);
    fprintf(stderr, "expected_ast_str: \"%s\"\n", expected_ast_str);

    assert(ast_str.length == expected_length);
    assert(string_equal(ast_str, expected_ast_str));

    printf("%s", ast_str.data);

    return 0;
}
