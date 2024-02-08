
#include <containers/darray.h>
#include <defines.h>
#include <memory/allocator.h>
#include <nstring.h>

#include <ast_print.h>
#include <atom.h>
#include <instance.h>
#include <options.h>
#include <parser.h>

#include <cstdio>
#include <cassert>
#include <cstring>

using namespace Novo;

int main(int argc, char* argv[])
{
    Instance instance;
    Options options = default_options();
    options.install_dir = "../../../";
    instance_init(&instance, options);

    Imported_File imported_file = {
        .path = atom_get("test.no"),
        .ast = nullptr,
        .newline_offsets = {},
    };
    darray_append(&instance.imported_files, imported_file);

    auto file = parse_file(&instance, "test.no", 0);
    assert(file);

    auto ast_str = ast_to_string(&instance, file, c_allocator());

    auto expected_ast_str = R"(test.no:002:001-007:001: FUNC_DECL: 'main'
test.no:002:015-0000003:  RETURN_TS:
test.no:002:015-0000003:   IDENT_TS: 'int'
test.no:002:019-007:001:  BODY:
test.no:003:005-0000008:   VAR_DECL: 'x'
test.no:003:010-0000003:    INIT:
test.no:003:010-0000003:     EXPR_CHAR: 'a'
test.no:004:005-0000010:   VAR_DECL: 'y'
test.no:004:010-0000005:    INIT:
test.no:004:010-0000005:     EXPR_STR: "abc"
test.no:005:005-0000007:   VAR_DECL: 'z'
test.no:005:010-0000002:    INIT:
test.no:005:010-0000002:     EXPR_INT: 42
test.no:006:005-0000008:   STMT_RETURN:
test.no:006:012-0000001:    EXPR_INT: 0

test.no:009:001-011:001: FUNC_DECL: 'add'
test.no:009:009-0000014:  PARAMS: 2
test.no:009:009-0000006:   VAR_DECL: 'a'
test.no:009:012-0000003:    TS:
test.no:009:012-0000003:     IDENT_TS: 'int'
test.no:009:017-0000006:   VAR_DECL: 'b'
test.no:009:020-0000003:    TS:
test.no:009:020-0000003:     IDENT_TS: 'int'
test.no:009:028-0000003:  RETURN_TS:
test.no:009:028-0000003:   IDENT_TS: 'int'
test.no:009:032-011:001:  BODY:
test.no:010:005-0000012:   STMT_RETURN:
test.no:010:012-0000005:    EXPR_BINARY: '+'
test.no:010:012-0000001:     EXPR_IDENT: 'a'
test.no:010:016-0000001:     EXPR_IDENT: 'b'

test.no:013:001-020:001: FUNC_DECL: 'test'
test.no:013:015-0000003:  RETURN_TS:
test.no:013:015-0000003:   IDENT_TS: 'int'
test.no:013:019-020:001:  BODY:
test.no:014:005-0000007:   VAR_DECL: 'x'
test.no:014:009-0000003:    TS:
test.no:014:009-0000003:     IDENT_TS: 'int'
test.no:015:005-0000007:   VAR_DECL: 'y'
test.no:015:009-0000003:    TS:
test.no:015:009-0000003:     IDENT_TS: 'int'
test.no:016:005-0000007:   VAR_DECL: 'z'
test.no:016:009-0000003:    TS:
test.no:016:009-0000003:     IDENT_TS: 'int'
test.no:018:005-0000018:   VAR_DECL: 'r'
test.no:018:010-0000013:    INIT:
test.no:018:010-0000013:     EXPR_BINARY: '+'
test.no:018:010-0000005:      EXPR_BINARY: '*'
test.no:018:010-0000001:       EXPR_IDENT: 'x'
test.no:018:014-0000001:       EXPR_IDENT: 'y'
test.no:018:018-0000005:      EXPR_BINARY: '/'
test.no:018:018-0000001:       EXPR_IDENT: 'z'
test.no:018:022-0000001:       EXPR_INT: 2
test.no:019:005-0000021:   VAR_DECL: 'r2'
test.no:019:011-0000015:    INIT:
test.no:019:011-0000015:     EXPR_BINARY: '/'
test.no:019:011-0000010:      EXPR_BINARY: '*'
test.no:019:011-0000001:       EXPR_IDENT: 'x'
test.no:019:016-0000005:       EXPR_BINARY: '+'
test.no:019:016-0000001:        EXPR_IDENT: 'y'
test.no:019:020-0000001:        EXPR_IDENT: 'z'
test.no:019:025-0000001:      EXPR_INT: 2

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
