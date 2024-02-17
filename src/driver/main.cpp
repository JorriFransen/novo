
#include <containers/darray.h>
#include <defines.h>
#include <instance.h>
#include <logger.h>

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

    u64 o = 1;
    u64 a = 8;

    u64 r = get_aligned(o, a);

    log_info("get_aligned(%llu, %llu) = %llu", o, a, r);
    return 0;
}
