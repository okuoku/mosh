/* dummy_main for tests */

#include <time.h>
#include <signal.h>
#include <stdio.h>
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "scheme.h"
#include "VM.h"
#include "SString.h"
#include "Symbol.h"
#include "getoptU.h"
#include "OSCompat.h"
#include "OSCompatThread.h"
#include "MultiVMProcedures.h"
#include "VMFactory.h"
#include "Gloc.h"
#include "Closure.h"
#include "VM-inl.h"

using namespace scheme;

/* Dummy definitions for R6RS frontend-less tests */
Object
activateR6RSMode(VM* vm, bool bogus){
    abort();
    return Object::Nil;
}
