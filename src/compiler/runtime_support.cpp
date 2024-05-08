#include "runtime_support.h"

extern "C" {

s32 foreign_add(s64 a, s64 b) {
    return a + b;
}

}
