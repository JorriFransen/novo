#include <instance.h>

using namespace Novo;

int main(int argc, char *argv[])
{
    Instance instance;
    instance_init(&instance);

    if (!instance_start(&instance, "test/test.no")) {
        return 1;
    }

    return 0;
}
