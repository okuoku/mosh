// nmosh plugin helper
//  Plugin interface for mosh VM/heap

#include <nmosh/plugin-if.h>

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

extern VM* theVM; // Global root VM
extern Object activateR6RSMode(VM* theVM, bool isDebugExpand);

// Exported functions
extern "C" {

// FIXME: Copied from MultiVMProcedures.cpp, integrate them

void*
moshvm_alloc(void){
    VMFactory fac;
    const int INITIAL_STACK_SIZE = 5000;
    const bool isProfilerOn = false;
    VM* vm = fac.create(INITIAL_STACK_SIZE, isProfilerOn);
    VM::copyOptions(vm, theVM);
    return reinterpret_cast<void*>(vm);
}

void // FIXME: fail?
moshvm_set_value_string(void* pvm, const char* symname, const char* value){
    VM* vm = (VM *)pvm;
    vm->setValueString(UC(symname), value);
}

void // FIXME: fail?
moshvm_set_value_pointer(void* pvm, const char* symname, void* value){
    VM* vm = (VM *)pvm;
    vm->setValueString(UC(symname), Object::makePointer(value));
}

void // FIXME: fail?
moshvm_set_value_boolean(void* pvm, const char* symname, int b){
    VM* vm = (VM *)pvm;
    vm->setValueString(UC(symname), Object::makeBool(b?true:false));
}

static void*
entry(void* pvm){
    VM* vm = (VM *)pvm;
    // setCurrentVM(vm); // FIXME: Implement it
    Object ret = activateR6RSMode(vm, false);
    return reinterpret_cast<void*>(new Object(ret));
}

void
moshvm_start(void* pvm, int bogus /* prio */, const char* threadname){
    Thread* thread = new Thread();
    VM* vm = (VM *)pvm;
    // Initialize VM data
    vm->setThread(thread);
    if(threadname){
        vm->setName(ucs4string::from_c_str(threadname));
    }
    // Kick VM thread
    thread->create(entry, vm, Thread::priorityNormal, threadname);
}

void*
moshvm_join(void* pvm){
    VM* vm = (VM *)pvm;
    void* p;
    vm->thread()->join(&p);
    return p;
}

uintptr_t // Called from embedded plugins
moshvm_callback_call(void* p, void* l){
    Object& obj = Object::makeRaw(p);
    Object& lis = Object::makeRaw(l);
    VM* theVM = obj.toPair()->car.toVM();
    Object& ret = theVM->apply(obj.toPair()->cdr, lis);
    return (uintptr_t)ret.toPointer()->pointer();
}

void*
moshvm_export_object(const nmosh_export_entry_t en[]){
    Object tmp;
    // For debug
    void* p;
    double d;
    tmp = Object::Nil;
    for(int i = 0; en[i].type != NMOSH_EXPORT_TYPE_TERM ; i++){
        Object me;
        switch(en[i].type){
            case NMOSH_EXPORT_TYPE_INT:
                me = Object::cons(Object::makeString(en[i].name),
                                  Object::makeBignum(en[i].arg0));
                break;
            case NMOSH_EXPORT_TYPE_DOUBLE:
                d = *(double *)en[i].arg0;
                me = Object::cons(Object::makeString(en[i].name),
                                  Object::makeFlonum(d));
                break;
            case NMOSH_EXPORT_TYPE_CSTRING:
                me = Object::cons(Object::makeString(en[i].name),
                                  Object::makeString((const char *)en[i].arg0));
                break;
            case NMOSH_EXPORT_TYPE_BUFFER:
                me = Object::cons(Object::makeString(en[i].name),
                                  Object::makeByteVector((const char*)en[i].arg0 ,
                                                         en[i].arg1));
                break;
            case NMOSH_EXPORT_TYPE_STRUCT:
                p = moshvm_export_object((const nmosh_export_entry_t*)
                                         en[i].arg0);
                me = Object::cons(Object::makeString(en[i].name),
                                  Object::makeRaw(p));
                break;
            default:
                me = Object::Nil;
                break;
        }
        tmp = Object::cons(me, tmp);
    }
    return (void*)tmp.val;
}

} /* Extern C */
