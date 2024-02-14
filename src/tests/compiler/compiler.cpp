
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
    u64 return_code = 0;
    bool assert_fail = false;
};

static Test_Case test_cases[] = {
    { .file_path = "tests/001_assert_false.no", .assert_fail = true },
    { .file_path = "tests/002_return.no" },
    { .file_path = "tests/003_call.no" },
    { .file_path = "tests/004_structs.no" },
    { .file_path = "tests/005_if.no" },

    { .file_path = "tests/006_while.no" },
    { .file_path = "tests/007_for.no" },
    { .file_path = "tests/008_arithmetic_assignment.no" },
    { .file_path = "tests/009_break_while_for.no" },
    { .file_path = "tests/010_continue_while_for.no" },
    { .file_path = "tests/011_struct_compound.no" },
    { .file_path = "tests/012_nested_struct_compound.no" },
    { .file_path = "tests/013_constant_struct_compound.no" },
    { .file_path = "tests/014_pointers.no" },
    { .file_path = "tests/015_struct_pointers.no" },
    { .file_path = "tests/016_foreign_functions.no" },

    { .file_path = "tests/017_trunc.no" },
    { .file_path = "tests/018_sext.no" },
    { .file_path = "tests/019_zext.no" },
};

static bool run_test_case(Test_Case* tc)
{
    Options options = default_options();
    options.install_dir = "../../../";
    // options.print_ast = false;
    // options.print_bytecode = true;
    options.input_file = tc->file_path;

    Instance instance;
    instance_init(&instance, options);

    if (!instance_start(&instance)) {
        return false;
    }

    assert(instance.ssa_program->entry_fn_index >= 0);

    VM_Result run_result = instance.entry_run_result;

    bool result = false;

    if (tc->assert_fail && !run_result.assert_fail) {
        fprintf(stderr, "Expected assert failure in test '%s', (returned: %llu)\n", tc->file_path, run_result.return_value);

    } else if (!tc->assert_fail && run_result.assert_fail) {
        fprintf(stderr, "Unexpected assert failure in test '%s', (returned: %llu)\n", tc->file_path, run_result.return_value);

    } else if (!tc->assert_fail && tc->return_code != run_result.return_value) {
        fprintf(stderr, "Mismatching return code for test file '%s', got: %llu, expected: %llu\n", tc->file_path, run_result.return_value, tc->return_code);

    } else {
        result = true;
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
