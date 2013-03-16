// nmosh interrupt I/F
//  Plugin interface for nmosh interrupt

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

// Terminal signals
#ifdef _WIN32
#include <windows.h>
#else
// FIXME: Implement it
#endif

using namespace scheme;
extern VM* theVM; // Global root VM

extern "C" {
// }

// ------------------------------------------------------------------
// -- Core
// ------------------------------------------------------------------

void // Exported
moshvm_interrupt(VM* vm){
    vm->InterruptNmosh();
}

static void // Chime interface to interrupt main VM execution
interrupt_main_vm(void* bogus){
    theVM->InterruptNmosh();
}

// ------------------------------------------------------------------
// -- Terminal signals
// ------------------------------------------------------------------

#if defined(_WIN32)
static BOOL WINAPI
interrupted(DWORD type){
    // NB: Invoked from hidden thread.
    if(type == CTRL_C_EVENT || type == CTRL_BREAK_EVENT){
        interrupt_main_vm(NULL);
        return TRUE;
    }else{
        return FALSE;
    }
}
void // Exported
moshvm_keyboard_interrupt_enable(void){
    SetConsoleCtrlHandler(interrupted, TRUE);
}
#else
// Generic null implementation
void // Exported
moshvm_keyboard_interrupt_enable(void){
    /* Do nothing */
}
#endif

// {
};
