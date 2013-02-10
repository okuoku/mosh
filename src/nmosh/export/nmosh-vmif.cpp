// nmosh heap interface

#include <nmosh/vm-if.h>
#include "nmosh-c.h"

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "ByteVector.h"
#include "VM.h"
#include "ByteVectorProcedures.h"
#include "ErrorProcedures.h"
#include "PortProcedures.h"
#include "ByteArrayBinaryInputPort.h"
#include "BinaryInputPort.h"
#include "Symbol.h"
#include "ProcedureMacro.h"
#include "Transcoder.h"
#include "UTF8Codec.h"
#include "UTF16Codec.h"
#include "UTF32Codec.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Bignum.h"
#include "Arithmetic.h"
#include "TextualOutputPort.h"
#include "FFI.h"
#include "Gloc.h"
#include "Closure.h"
#include "VM-inl.h"
#include "VMFactory.h"
#include "OSCompatThread.h"

using namespace scheme;

void*
build_init_param(nmosh_vm_s* par){
    NMOSH_EXPORT_BEGIN(p)
        NMOSH_EXPORT_POINTER("library-load", &par->library_load)
        NMOSH_EXPORT_POINTER("library-lookup", &par->library_lookup)
    NMOSH_EXPORT_END()
    return moshvm_export_object(p);
}

extern "C" {
// }

/* CALLBACK(Closure) */
NMOSHDLL
void
nmosh_callback_destroy(nmosh_object_t* callback){
}

NMOSHDLL
int /* NMOSH_SUCCESS for success, otherwise error */
nmosh_callback_create(nmosh_callback_func_t func, void* ctx,
                      nmosh_object_t* out_callback){
    return NMOSH_SUCCESS;
}

/* LIBRARY */
NMOSHDLL
int /* NMOSH_SUCCESS for success, otherwise error */
nmosh_library_lookup(nmosh_vm_t vm, const char* libname,
                     const char* symbol, nmosh_object_t* out_object){
    return NMOSH_SUCCESS;

}

NMOSHDLL
int /* NMOSH_SUCCESS for success, otherwise error */
nmosh_library_load(nmosh_vm_t vm, const char* libname){
    return NMOSH_SUCCESS;
}

/* VM */
NMOSHDLL
void
nmosh_vm_destroy(nmosh_vm_t vm){
    GC_FREE(vm);
}

NMOSHDLL
int /* NMOSH_SUCCESS for success, otherwise error */
nmosh_vm_create(nmosh_vmattr_t* attr, nmosh_vm_t* out_vm){
    void* obj;
    nmosh_vm_s* nvm; /* = nmosh_vm_t */
    nvm = (nmosh_vm_s*)GC_MALLOC_UNCOLLECTABLE(sizeof(nmosh_vm_s));
    obj = build_init_param(nvm);
    nvm->vm = (nmosh_vm_t)moshvm_alloc();
    *out_vm = nvm;
    /* FIXME: Handle error */
    moshvm_launch(nvm->vm, obj);
    return 0;
}

NMOSHDLL
int
nmosh_init(void){
    ::mosh_init();
    return 0;
}

// {
};
