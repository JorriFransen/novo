
#include <ast.h>
#include <ast_print.h>
#include <cstdio>
#include <instance.h>
#include <memory/allocator.h>
#include <parser.h>

#include <cassert>

using namespace Novo;

int main(int argc, char *argv[])
{
    Instance instance;
    instance_init(&instance);

    auto file = parse_file(&instance, "/home/jorri/dev/novo/src/tests/parser/test.no");
    assert(file);

    auto ast_str = ast_to_string(file, c_allocator());

    auto expected_ast_str = R"(FUNC_DECL: 'main'
 PARAMS: 0
 BODY:
  VAR_DECL: 'x'
   EXPR_CHAR: 'a'
  VAR_DECL: 'y'
   EXPR_STR: "abc"
  VAR_DECL: 'z'
   EXPR_INT: 42
  STMT_RETURN:
   EXPR_INT: 0

FUNC_DECL: 'add'
 PARAMS: 2
  VAR_DECL: 'a'
   IDENT_TS: 'int'
  VAR_DECL: 'b'
   IDENT_TS: 'int'
 BODY:
  STMT_RETURN:
   EXPR_BINARY: '+'
    EXPR_IDENT: 'a'
    EXPR_IDENT: 'b'

FUNC_DECL: 'test'
 PARAMS: 0
 BODY:
  VAR_DECL: 'x'
   IDENT_TS: 'int'
  VAR_DECL: 'y'
   IDENT_TS: 'int'
  VAR_DECL: 'z'
   IDENT_TS: 'int'
  VAR_DECL: 'r'
   EXPR_BINARY: '+'
    EXPR_BINARY: '*'
     EXPR_IDENT: 'x'
     EXPR_IDENT: 'y'
    EXPR_BINARY: '/'
     EXPR_IDENT: 'z'
     EXPR_INT: 2
  VAR_DECL: 'r2'
   EXPR_BINARY: '/'
    EXPR_BINARY: '*'
     EXPR_IDENT: 'x'
     EXPR_BINARY: '+'
      EXPR_IDENT: 'y'
      EXPR_IDENT: 'z'
    EXPR_INT: 2

)";

    auto expected_length = strlen(expected_ast_str);
    assert(ast_str.length == expected_length);
    assert(string_equal(ast_str, expected_ast_str));

    printf("%s", ast_str.data);

    return 0;
}
