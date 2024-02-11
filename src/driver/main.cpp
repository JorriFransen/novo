
#include <defines.h>
#include <instance.h>
#include <logger.h>
#include <memory/allocator.h>
#include <vm.h>

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

    VM vm;
    vm_init(&vm, c_allocator(), &instance);

    VM_Result vr = vm_run(&vm, instance.ssa_program);
    if (vr.assert_fail) {
        log_warn("Bytecode vm quit after failed assert");
    }
    log_info("Bytecode vm returned: %llu\n", vr.return_value);

    return 0;
}
