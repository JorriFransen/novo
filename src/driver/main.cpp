
#include <defines.h>
#include <instance.h>
#include <logger.h>
#include <memory/allocator.h>
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

    #ifdef NOVO_TRACE_ALLOC
        report_allocator_trace();
    #endif //NOVO_TRACE_ALLOC
    return 0;
}
