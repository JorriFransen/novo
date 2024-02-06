#include "runtime_support.h"

extern "C" {

NAPI s32 foreign_add(s64 a, s64 b) {
    return a + b;
}

}
