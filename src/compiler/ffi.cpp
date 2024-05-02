#include "ffi.h"

#include <containers/darray.h>
#include <dyncall.h>
#include <dynload.h>

#include "atom.h"
#include "instance.h"

#include <nstring.h>

#include <assert.h>

namespace Novo {

struct FFI_Lib
{
    Atom name;
    DLLib* lib;
};

void ffi_init(Instance* inst, FFI* ffi, Allocator* allocator)
{
    ffi->allocator = allocator;


    ffi->vm = dcNewCallVM(4096);
    dcMode(ffi->vm, DC_CALL_C_DEFAULT);

    darray_init(allocator, &ffi->libs);
    darray_init(allocator, &ffi->functions);

    ffi_load_library(ffi, inst->support_lib_d_path);

#if NPLATFORM_LINUX

    DLLib* this_lib = dlLoadLibrary(nullptr);
    assert(this_lib);
    darray_append(&ffi->libs, { atom_get(".self."), this_lib });

#elif NPLATFORM_WINDOWS

    ffi_load_library(ffi, "novo_compiler.dll");
    // String_Ref novo_common_name = "novo_compiler.dll";
    // DLLib* novo_common_lib = dlLoadLibrary(novo_common_name.data);
    // assert(novo_common_lib);
    // darray_append(&ffi->libs, { atom_get(novo_common_name), novo_common_lib });

    ffi_load_library(ffi, "msvcrt.dll");
    // String_Ref msvcrt_name = "msvcrt.dll";
    // DLLib* msvcrt_lib = dlLoadLibrary(msvcrt_name.data);
    // assert(msvcrt_lib);
    // darray_append(&ffi->libs, { atom_get(msvcrt_name), msvcrt_lib });

#endif // NPLATFORM_...
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

bool ffi_load_library(FFI* ffi, String_Ref name)
{
    NSTRING_ASSERT_ZERO_TERMINATION(name);

    DLLib* lib = dlLoadLibrary(name.data);
    if (!lib) return false;

    darray_append(&ffi->libs, { atom_get(name.data), lib });
    return true;
}

}
