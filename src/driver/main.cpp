
#include <defines.h>
#include <instance.h>

#include "command_line_args.h"

#ifdef NOVO_TRACE_ALLOC
#   include <memory/c_allocator.h>
#   include <memory/freelist.h>
#   include <memory/trace.h>
#endif // NOVO_TRACE_ALLOC

using namespace Novo;

int main(int argc, char* argv[])
{
    auto options = parse_command_line(argc, argv);

    Instance instance;
    instance_init(&instance, options);

    if (!instance_start(&instance)) {
        return 1;
    }

    #ifdef NOVO_TRACE_ALLOC
        instance_free(&instance);
        release(c_allocator(), options.output);
        free_atoms();

        // report_allocator_trace("c_allocator()", (Allocator_Trace*)c_allocator()->user_data);
        report_allocator_trace("fl_allocator()", &((Freelist*)fl_allocator()->user_data)->trace);
    #endif //NOVO_TRACE_ALLOC
    return 0;
}
