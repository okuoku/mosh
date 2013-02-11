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

Object
objref(nmosh_object_t obj){
    return Object::makeRaw(*(void **)obj);
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
    void* in;
    NMOSH_EXPORT_BEGIN(param)
        NMOSH_EXPORT_CSTRING(NULL, libname)
        NMOSH_EXPORT_CSTRING(NULL, symbol)
    NMOSH_EXPORT_END()
    in = moshvm_export_object(param);
    return nmosh_vm_apply(vm, &vm->library_lookup, &in, out_object);
}

NMOSHDLL
int /* NMOSH_SUCCESS for success, otherwise error */
nmosh_library_load(nmosh_vm_t vm, const char* libname){
    return NMOSH_SUCCESS;
}

/* OBJECT */
NMOSHDLL
int
nmosh_object_export(nmosh_export_entry_t* e, nmosh_object_t* out){
    void** v;
    v = (nmosh_object_t*)GC_MALLOC_UNCOLLECTABLE(sizeof(void*));
    *v = moshvm_export_object(e);
    *out = v;
    return NMOSH_SUCCESS;
}

NMOSHDLL
void
nmosh_object_destroy(nmosh_object_t obj){
    GC_FREE(obj);
}

/* VM */
NMOSHDLL
int
nmosh_vm_apply(nmosh_vm_t vm, nmosh_object_t closure, nmosh_object_t arg,
               nmosh_object_t* out_obj){
    VM* theVM;
    theVM = (VM *)vm->vm;
    const Object& ret = theVM->apply(objref(closure),objref(arg));
    if(out_obj){
        void** out;
        void* obj;
        obj = (void*)ret.val;
        out = (void**)GC_MALLOC_UNCOLLECTABLE(sizeof(void*));
        *out = obj;
        *out_obj = out;
    }
    return NMOSH_SUCCESS;
}

NMOSHDLL
void
nmosh_vm_destroy(nmosh_vm_t vm){
    GC_FREE(vm);
}

NMOSHDLL
int /* NMOSH_SUCCESS for success, otherwise error */
nmosh_vmattr_setloadpath(nmosh_vmattr_t attr, const char* loadpath){
    attr->loadpath = GC_STRDUP(loadpath);
    return NMOSH_SUCCESS;
}

NMOSHDLL
int
nmosh_vmattr_setverbose(nmosh_vmattr_t attr, int verbose){
    attr->verbose = verbose;
    return NMOSH_SUCCESS;
}

NMOSHDLL
void
nmosh_vmattr_destroy(nmosh_vmattr_t attr){
    GC_FREE(attr);
}

NMOSHDLL
int
nmosh_vmattr_init(nmosh_vmattr_t* out_attr){
    nmosh_vmattr_s* attr;
    attr = (nmosh_vmattr_s*)GC_MALLOC_UNCOLLECTABLE(sizeof(nmosh_vmattr_s));
    attr->verbose = 0;
    attr->loadpath = NULL;
    *out_attr = (nmosh_vmattr_t)attr;
    return NMOSH_SUCCESS;
}

NMOSHDLL
int /* NMOSH_SUCCESS for success, otherwise error */
nmosh_vm_create(nmosh_vmattr_t attr, nmosh_vm_t* out_vm){
    void* obj;
    nmosh_vm_s* nvm; /* = nmosh_vm_t */
    nvm = (nmosh_vm_s*)GC_MALLOC_UNCOLLECTABLE(sizeof(nmosh_vm_s));
    obj = build_init_param(nvm);
    nvm->vm = (nmosh_vm_t)moshvm_alloc();
    /* setup parameters */
    if(attr && attr->loadpath){
        moshvm_set_value_string(nvm->vm, "%loadpath", attr->loadpath);
    }else{
        moshvm_set_value_boolean(nvm->vm, "%loadpath", 0);
    }
    if(attr && attr->verbose){
        moshvm_set_value_boolean(nvm->vm, "%verbose", 1);
    }else{
        moshvm_set_value_boolean(nvm->vm, "%verbose", 0);
    }
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
