
#include <containers/darray.h>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <defines.h>
#include <instance.h>
#include <logger.h>

#include "command_line_args.h"

using namespace Novo;

#define TIMED(name, code) { \
    clock_t start = clock(), diff; \
    (code); \
    diff = clock() - start; \
    int msec = diff * 1000 / CLOCKS_PER_SEC; \
    printf("TIMED(" name ")\t%d.%d\n", msec/1000, msec%1000); \
    acc += msec; \
}


// #define C 10000000
#define C 30000000
    int ints_[C];

int main(int argc, char* argv[])
{
    auto options = parse_command_line(argc, argv);

    Instance instance;
    instance_init(&instance, options);

    if (!instance_start(&instance)) {
        return 1;
    }

    instance_free(&instance);

    srand(time(NULL));

#define RANDOMIZE_ARRAY \
    for (int i = 0; i < C; i++) { \
        ints_[i] = rand(); \
    } \

    Array_Ref<int> ints = ints_;

#define CHECK_ARRAY { \
    s64 last_index = -1; \
    for (s64 i = 0; i < ints.count; i++) { \
        if (last_index >= 0) { \
            assert(ints[last_index] <= ints[i]); \
        } \
        last_index = i; \
    } }\


#define TIMED_AVG(name, code, count) { \
    int acc = 0; \
    for (int i = 0; i < (count); i++) { \
        RANDOMIZE_ARRAY \
        TIMED(name, (code)); \
        CHECK_ARRAY \
    } \
    acc /= (count); \
    printf("AVG(" name ", %d)\t%d.%d\n\n", (count), acc/1000, acc%1000); \
} \


    TIMED_AVG("quicksort 1", quicksort1(ints), 1); // 3.174
    TIMED_AVG("quicksort 2", quicksort2(ints), 1); // 3.174

    Array_Ref<int> arr({ 7, 6, 4, 3, 9, 2, 11, 1, 5});
    quicksort2(arr);

    // for (s64 i = 0; i < arr.count; i++) {
    //     printf("%d\n", arr[i]);
    // }


    // { 2, 4, 1 };

    return 0;
}
