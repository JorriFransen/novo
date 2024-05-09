
#include <defines.h>
#include <instance.h>
#include <logger.h>
#include <string_builder.h>

#include "command_line_args.h"

using namespace Novo;

int main(int argc, char* argv[])
{
    auto options = parse_command_line(argc, argv);

    Instance instance;
    instance_init(&instance, options);

    if (!instance_start(&instance)) {
        return 1;
    }

    instance_free(&instance);
    return 0;

    // Array_Ref<String_Ref> command({"test.bat"});
    // Command_Result run_result = platform_run_command(command, temp_allocator());
    //
    // if (run_result.result_string.length) {
    //     printf("stdout: '%.*s'", (int)run_result.result_string.length, run_result.result_string.data);
    // }
    //
    // if (run_result.error_string.length) {
    //     printf("\nstderr: '%.*s'", (int)run_result.error_string.length, run_result.error_string.data);
    // }

    return 0;
}
