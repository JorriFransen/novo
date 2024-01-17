
#include <defines.h>
#include <instance.h>

#include "command_line_args.h"

using namespace Novo;

int main(int argc, char *argv[])
{
    parse_command_line(argc, argv);

    Instance instance;
    instance_init(&instance);

    if (!instance_start(&instance, "test/test.no")) {
        return 1;
    }

    return 0;
}
