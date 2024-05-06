#include "backend.h"

#include "c_backend.h"

namespace Novo {

bool backend_emit(Instance* inst) {
    return c_backend_emit(inst);
}

}
