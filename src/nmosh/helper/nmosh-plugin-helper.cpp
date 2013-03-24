// nmosh plugin helper
//  Plugin interface for mosh VM/heap

#undef NMOSHPLUGIN_EMBED // FIXME: Too hacky
#include <nmosh/plugin-if.h>
#include "nmosh-c.h"
#include <setjmp.h>

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
#include "MultiVMProcedures.h"

using namespace scheme;

extern VM* theVM; // Global root VM
extern Object activateR6RSMode(VM* theVM, bool isDebugExpand);


static Object
objcons(const char* name, Object o){
    if(!name){
        return o;
    }else{
        return Object::cons(Object::makeString(name),
                            o);
    }
}


// FIXME: Copied from main.cpp
static Object
internalGetStackTraceObj(VM* vm, int argc, const Object* argv){
        //DeclareProcedureName("%get-stack-trace-obj");
        return vm->getStackTraceObj();
}
static Object
internalGetNmoshDbgImage(VM* vm, int argc, const Object* argv){
    //DeclareProcedureName("%get-nmosh-dbg-image");
    return Object::Nil;
    //return FaslReader(vm, new ByteArrayBinaryInputPort(nmosh_dbg_image_ptr, nmosh_dbg_image_size)).get();
}

void* shared_hashtable_obj = NULL;

// Exported functions
extern "C" {


// FIXME: Copied from MultiVMProcedures.cpp, integrate them

void*
moshvm_profile_get_result(VM* vm){
#ifdef ENABLE_PROFILER
    const Object& obj = vm->getProfileResult();
#else
    const Object& obj = Object::False;
#endif
    return (void*)obj.val;
}

void
moshvm_longjmp(jmp_buf* jbp){
    longjmp(*jbp,1);
}

int /* 0 for SUCCESS */
moshvm_launch(void* pvm,void* param){
    jmp_buf jb;
    VM* vm = (VM *)pvm;
    switch(setjmp(jb)){
        case 0: /* Launch VM */
            /* FIXME: Magic */
            vm->setValueString(UC("%get-stack-trace-obj"),
               Object::makeCProcedure(internalGetStackTraceObj));
            vm->setValueString(UC("%get-nmosh-dbg-image"),
               Object::makeCProcedure(internalGetNmoshDbgImage));

            moshvm_set_value_boolean(pvm, "*command-line-args*", 0);
            moshvm_set_value_boolean(pvm, "%disable-acc", 0);
            moshvm_set_value_boolean(pvm, "%nmosh-preload-mode", 0);
            moshvm_set_value_boolean(pvm, "%nmosh-prefixless-mode", 1);
            moshvm_set_value_boolean(pvm, "%nmosh-portable-mode", 1);
            moshvm_set_value_pointer(pvm, "%nmosh-dll-param", param);
            moshvm_set_value_pointer(pvm, "%nmosh-dll-jmpbuf", &jb);
            moshvm_set_value_pointer(pvm, "%nmosh-self", pvm);
            setCurrentVM(vm);
            /* Launch library initialize */
            /* FIXME: Object ret = */ activateR6RSMode(vm, false);
            return -2; /* FAIL */
        case 1:
            return 0; /* SUCCESS */
        default:
            return -1; /* FAIL */
    }
}

void*
moshvm_alloc(void){
    VMFactory fac;
    const int INITIAL_STACK_SIZE = 5000;
    const bool isProfilerOn = false;
    VM* vm = fac.create(INITIAL_STACK_SIZE, isProfilerOn);
    if(theVM){
        VM::copyOptions(vm, theVM);
    }
    vm->setValueString(UC("%get-stack-trace-obj"),
            Object::makeCProcedure(internalGetStackTraceObj));
    vm->setValueString(UC("%get-nmosh-dbg-image"),
            Object::makeCProcedure(internalGetNmoshDbgImage));
    return reinterpret_cast<void*>(vm);
}

void // FIXME: fail?
moshvm_set_value_string(void* pvm, const char* symname, const char* value){
    VM* vm = (VM *)pvm;
    vm->setValueString(ucs4string::from_c_str(symname).strdup(), value);
}

void // FIXME: fail?
moshvm_set_value_pointer(void* pvm, const char* symname, void* value){
    VM* vm = (VM *)pvm;
    vm->setValueString(ucs4string::from_c_str(symname).strdup(), 
                       Object::makePointer(value));
}

void // FIXME: fail?
moshvm_set_value_boolean(void* pvm, const char* symname, int b){
    VM* vm = (VM *)pvm;
    vm->setValueString(ucs4string::from_c_str(symname).strdup(), 
                       Object::makeBool(b?true:false));
}

static void*
entry(void* pvm){
    VM* vm = (VM *)pvm;
    setCurrentVM(vm);
    Object ret = activateR6RSMode(vm, false);
    return reinterpret_cast<void*>(new Object(ret));
}

void
core_moshvm_start(void* pvm, int bogus /* prio */, const char* threadname,
                  void* ticket_func, void* ticket_data){
    Thread* thread = new Thread();
    VM* vm = (VM *)pvm;
    // Initialize VM data
    vm->setThread(thread);
    if(threadname){
        vm->setName(ucs4string::from_c_str(threadname));
    }
    // Kick VM thread
    thread->create(entry, vm, Thread::priorityNormal, threadname,
                   ticket_func, ticket_data);
}

void
moshvm_start(void* pvm, int bogus /* prio */, const char* threadname){
    core_moshvm_start(pvm,bogus,threadname,NULL,NULL);
}

void
moshvm_start_detached(void* pvm, int bogus /* prio */, const char* threadname,
                      void* ticket_func, void* ticket_data){
    core_moshvm_start(pvm,bogus,threadname,ticket_func,ticket_data);
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
    const Object& obj = Object::makeRaw(p);
    const Object& lis = Object::makeRaw(l);
    VM* theVM = obj.toPair()->car.toVM();
    const Object& ret = theVM->apply(obj.toPair()->cdr, lis);
    return (uintptr_t)ret.toPointer()->pointer();
}

uintptr_t
moshvm_apply(VM* pvm, void* p, void* l){
    const Object& obj = Object::makeRaw(p);
    const Object& lis = Object::makeRaw(l);
    const Object& ret = pvm->apply(obj, lis);
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
                me = objcons(en[i].name, Object::makeBignum(en[i].arg0));
                break;
            case NMOSH_EXPORT_TYPE_DOUBLE:
                d = *(double *)en[i].arg0;
                me = objcons(en[i].name, Object::makeFlonum(d));
                break;
            case NMOSH_EXPORT_TYPE_CSTRING:
                me = objcons(en[i].name, 
                             Object::makeString((const char*)en[i].arg0));
                break;
            case NMOSH_EXPORT_TYPE_BUFFER:
                me = objcons(en[i].name,
                             Object::makeByteVector((const char*)en[i].arg0,
                                                    (size_t)en[i].arg1));

                break;
            case NMOSH_EXPORT_TYPE_STRUCT:
                p = moshvm_export_object((const nmosh_export_entry_t*)
                                         en[i].arg0);
                me = objcons(en[i].name,
                             Object::makeRaw(p));
                break;
            case NMOSH_EXPORT_TYPE_POINTER:
                me = objcons(en[i].name,
                             Object::makePointer((void*)en[i].arg0));
                break;
            default:
                me = Object::Nil;
                break;
        }
        tmp = Object::cons(me, tmp);
    }
    return (void*)Pair::reverse(tmp).val;
}

// Shared storage
void
moshvm_sharedstorage_init(void* p){
    shared_hashtable_obj = p;
}

void
moshvm_sharedstorage_get(void* proc){
    NMOSH_EXPORT_BEGIN(ret)
        NMOSH_EXPORT_POINTER(NULL, &shared_hashtable_obj)
    NMOSH_EXPORT_END()
    // FIXME: Lock
    moshvm_callback_call(proc, moshvm_export_object(ret));
    // FIXME: Unlock
}

void*
moshvm_stacktrace_get(VM* pvm){
    return (void*)(pvm->getStackTraceObj().val);
}

void*
moshvm_getmainvm(void){
    return (void*)theVM;
}

void*
moshvm_getcurrentvm(void){
    return (void*)currentVM();
}

typedef void* (*xcallback_t)(void* ctx);

void*
moshvm_execute_callback(xcallback_t fn, void* param){
    return fn(param);
}

} /* Extern C */
