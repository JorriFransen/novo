#include "ffi.h"

#include <dyncall.h>
#include <dynload.h>

#include "atom.h"

#include <nstring.h>

#include <assert.h>

namespace Novo {

struct FFI_Lib
{
    Atom name;
    DLLib* lib;
};

void ffi_init(FFI* ffi, Allocator* allocator)
{
    ffi->allocator = allocator;


    ffi->vm = dcNewCallVM(4096);
    dcMode(ffi->vm, DC_CALL_C_DEFAULT);

    darray_init(allocator, &ffi->libs);
    darray_init(allocator, &ffi->functions);

    DLLib* this_lib = dlLoadLibrary(nullptr);
    assert(this_lib);
    darray_append(&ffi->libs, { atom_get(".self."), this_lib });
}

void ffi_free(FFI* ffi)
{
    darray_free(&ffi->libs);
    darray_free(&ffi->functions);
    dcFree(ffi->vm);
}

s64 ffi_load_function(FFI* ffi, Atom name)
{
    for (s64 i = 0; i < ffi->functions.count; i++) {
        if (ffi->functions[i].name == name) {
            return i;
        }
    }

    for (s64 i = 0; i < ffi->libs.count; i++) {

        void* sym = dlFindSymbol(ffi->libs[i].lib, atom_string(name).data);
        if (sym) {
            s64 result = ffi->functions.count;
            darray_append(&ffi->functions, { name, sym });
            return result;
        }
    }

    return -1;
}

}
