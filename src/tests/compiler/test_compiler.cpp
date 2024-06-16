
#include <defines.h>
#include <filesystem.h>
#include <logger.h>
#include <memory/arena.h>
#include <memory/c_allocator.h>
#include <memory/trace.h>
#include <nstring.h>
#include <platform.h>

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

static String test_path_prefix;

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

    { .file_path = "tests/020_pointer_math.no" },
    { .file_path = "tests/021_struct_align.no" },

    { .file_path = "tests/022_run_expression_trivial.no" },
    { .file_path = "tests/023_run_expression_trivial_multiple.no" },
    { .file_path = "tests/024_run_expression_aggregate.no" },
    { .file_path = "tests/025_run_expression_aggregate_nested.no" },
    { .file_path = "tests/026_run_statement.no", .return_code = 2 },

    { .file_path = "tests/027_insert_trivial.no", .return_code = 42 },
    { .file_path = "tests/028_insert_trivial_2.no", .return_code = 42 },
    { .file_path = "tests/029_insert_indirect.no", .return_code = 42 },

    { .file_path = "tests/030_global_variables.no", .return_code = 322 },
    { .file_path = "tests/031_constant_variables.no", .return_code = 66 },

    { .file_path = "tests/032_enums.no" },
};

static bool run_test_case(Test_Case* tc, Options options);

int main(int argc, char* argv[]) {

    Temp_Arena tarena = temp_arena(nullptr);
    Allocator ta = arena_allocator_create(tarena.arena);

    Options options;

    String exe_path = platform_exe_path(&ta, argv[0]);

    if (argc > 1) {
        test_path_prefix = string_copy(&ta, argv[1]);
        assert(fs_is_directory(test_path_prefix));
    }
    else {
        test_path_prefix = {};
    }

    String exe_dir = fs_dirname(&ta, exe_path);
    String build_dir_ = string_format(&ta, "%.*s" NPLATFORM_PATH_SEPARATOR "../../../", (int)exe_dir.length, exe_dir.data);
    assert(fs_is_directory(build_dir_.data));

    options.exe_dir = fs_realpath(&ta, build_dir_);

    s64 test_count = sizeof(test_cases) / sizeof(test_cases[0]);
    s64 test_success_count = 0;

    s64 max_it = 1;

    // #ifdef NOVO_TRACE_ALLOC
    //     max_it = 10;
    // #endif

    for (s64 it = 0; it < max_it; it++) {
        for (s64 i = 0; i < test_count; i++) {

            auto tc = &test_cases[i];

            printf("Running: '%s'...", tc->file_path);
            fflush(stdout);

            bool result = run_test_case(tc, options);

            printf("%s\n", result ? "OK" : "FAIL");
            fflush(stdout);

            if (result) test_success_count++;
        }

        printf("\n%lld/%lld tests successful\n", test_success_count, test_count);

    }

    temp_arena_release(tarena);
    int result = test_success_count == test_count * max_it ? 0 : 1;

    #ifdef NOVO_TRACE_ALLOC
        free_atoms();

        report_allocator_trace("c_allocator()", (Allocator_Trace*)c_allocator()->user_data);
    #endif // NOVO_TRACE_ALLOC

    return result;
}

static bool run_test_case(Test_Case* tc, Options options)
{
    options.keep_c_backend_output = true;
    options.input_file = tc->file_path;
    // options.verbose = true;
    // options.trace = true;

    Temp_Arena tarena = temp_arena(nullptr);
    Allocator ta = arena_allocator_create(tarena.arena);

    fs_mkdir("build");

    String filename = fs_filename_strip_extension(&ta, tc->file_path);
    options.output = string_format(&ta, "build" NPLATFORM_PATH_SEPARATOR "%.*s" NPLATFORM_DEFAULT_EXE_EXTENSION, (int)filename.length, filename.data).data;

    String_Ref test_path = tc->file_path;

    if (!fs_is_file(test_path)) {
        assert(test_path_prefix.length > 0 && "Expected test_path_prefix to be set");
        test_path = string_append(&ta, test_path_prefix, tc->file_path);
        assert(fs_is_file(test_path));
    }

    options.input_file = test_path.data;

    Instance inst;
    instance_init(&inst, options);


    if (!instance_start(&inst)) {
        return false;
    }

    assert(inst.ssa_program.entry_fn_index >= 0);

    VM_Result run_result = inst.entry_run_result;

    bool result = false;

    if (tc->assert_fail && !run_result.assert_fail) {
        fprintf(stderr, "Expected assert failure in test '%s', (returned: %llu)\n", test_path.data, run_result.return_value);

    } else if (!tc->assert_fail && run_result.assert_fail) {
        fprintf(stderr, "Unexpected assert failure in test '%s', (returned: %llu)\n", test_path.data, run_result.return_value);

    } else if (!tc->assert_fail && tc->return_code != run_result.return_value) {
        fprintf(stderr, "Mismatching return code for test file '%s', got: %llu, expected: %llu\n", test_path.data, run_result.return_value, tc->return_code);

    } else {
        result = true;
    }

    instance_free(&inst);

    temp_arena_release(tarena);

    return result;
}
