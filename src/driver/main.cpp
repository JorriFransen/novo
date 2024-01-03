#include <instance.h>

using namespace Novo;

int main(int argc, char *argv[])
{
    Instance instance;
    instance.first_file_name = "test/test.no";

    if (!instance_start(&instance)) {
        return 1;
    }

    return 0;
}
