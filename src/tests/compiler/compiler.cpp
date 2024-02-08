
#include <defines.h>
#include <memory/allocator.h>
#include <nstring.h>

#include <instance.h>
#include <options.h>
#include <ssa.h>
#include <vm.h>

#include <assert.h>
#include <stdio.h>

using namespace Novo;

struct Test_Case
{
    const char* file_path;
    u64 return_code;
};

static Test_Case test_cases[] = {
    { .file_path = "tests/001_return_immediate_0.no", .return_code = 0 },
    { .file_path = "tests/002_return_immediate_42.no", .return_code = 42 },
    { .file_path = "tests/003_return_immediate_bignum.no", .return_code = 0x102030405060708 },
    { .file_path = "tests/004_call.no", .return_code = 42 },
    { .file_path = "tests/005_call_2.no", .return_code = 42 },
    { .file_path = "tests/006_struct_basics.no", .return_code = 42 },
    { .file_path = "tests/007_struct_nested.no", .return_code = 84 },
    { .file_path = "tests/008_struct_assign.no", .return_code = 84 },
    { .file_path = "tests/009_struct_arg.no", .return_code = 66 },
    { .file_path = "tests/010_struct_return_1.no", .return_code = 22 },
    { .file_path = "tests/011_struct_return_2.no", .return_code = 33 },
    { .file_path = "tests/012_struct_return_3.no", .return_code = 66 },
    { .file_path = "tests/013_if_1.no", .return_code = 111 },
    { .file_path = "tests/014_if_2.no", .return_code = 18 },
    { .file_path = "tests/015_if_3.no", .return_code = 132 },
    { .file_path = "tests/016_while.no", .return_code = 42 },
    { .file_path = "tests/017_for.no", .return_code = 12 },
    { .file_path = "tests/018_arithmetic_assignment.no", .return_code = 5 },
    { .file_path = "tests/019_break_while_for.no", .return_code = 10 },
    { .file_path = "tests/020_continue_while_for.no", .return_code = 18 },
    { .file_path = "tests/021_struct_compound.no", .return_code = 41 },
    { .file_path = "tests/022_nested_struct_compound.no", .return_code = 10 },
    { .file_path = "tests/023_constant_struct_compound.no", .return_code = 120 },
    { .file_path = "tests/024_pointers.no", .return_code = 85 },
    { .file_path = "tests/025_struct_pointers.no", .return_code = 19 },
    { .file_path = "tests/026_foreign_functions.no", .return_code = 148 },
};

static bool run_test_case(Test_Case* tc)
{
    Options options = default_options();
    options.install_dir = "../../../";
    options.print_ast = true;
    options.input_file = tc->file_path;

    Instance instance;
    instance_init(&instance, options);

    if (!instance_start(&instance)) {
        return false;
    }

    assert(instance.ssa_program->entry_fn_index >= 0);

    VM vm;
    vm_init(&vm, c_allocator(), &instance);

    u64 return_code = vm_run(&vm, instance.ssa_program);

    bool result = return_code == tc->return_code;
    if (!result) {
        fprintf(stderr, "Mismatching return code for test file '%s', got: %llu, expected: %llu\n", tc->file_path, return_code, tc->return_code);
        result = false;
    }

    instance_free(&instance);

    return result;
}

int main(int argc, char* argv[]) {

    s64 test_count = sizeof(test_cases) / sizeof(test_cases[0]);
    s64 test_success_count = 0;

    for (s64 i = 0; i < test_count; i++) {

        auto tc = &test_cases[i];
        bool result = run_test_case(tc);

        printf("Running: '%s'... %s\n", tc->file_path, result ? "OK" : "FAIL");

        if (result) test_success_count++;
    }

    printf("\n%lld/%lld tests successful\n", test_success_count, test_count);

    return test_success_count == test_count ? 0 : 1;
}
