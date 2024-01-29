
#include <defines.h>
#include <instance.h>
#include <logger.h>
#include <memory/allocator.h>
#include <nstring.h>
#include <ssa.h>
#include <vm.h>

#include "command_line_args.h"

#include <cstdio>

using namespace Novo;

int main(int argc, char *argv[])
{
    auto options = parse_command_line(argc, argv);

    Instance instance;
    instance_init(&instance, options);

    if (!instance_start(&instance)) {
        return 1;
    }

    String ssa_str = ssa_to_string(c_allocator(), instance.ssa_program);
    printf("\n%s\n", ssa_str.data);

    VM vm;
    vm_init(&vm, c_allocator());

    auto r = vm_run(&vm, instance.ssa_program);
    log_info("Bytecode vm returned: %llu\n", r);

    return 0;
}
