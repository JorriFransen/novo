#pragma once

#include <atom.h>
#include <containers/darray.h>
#include <defines.h>

typedef struct DCCallVM_    DCCallVM;
typedef struct DLLib_       DLLib;

namespace Novo {

struct Allocator;
struct FFI_Lib;
struct Instance;

struct FFI_Function
{
    Atom name;
    void* sym;
};


struct FFI
{
    Allocator *allocator;
    DCCallVM* vm;

    DArray<FFI_Lib> libs;
    DArray<FFI_Function> functions;
};


NAPI void ffi_init(Instance *inst, FFI* ffi, Allocator* allocator);
NAPI void ffi_free(FFI* ffi);

NAPI s64 ffi_load_function(FFI* ffi, Atom name);
NAPI bool ffi_load_library(FFI* ffi, String_Ref name);

}
