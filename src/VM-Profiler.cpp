/*
 * VM-Profiler.cpp - 
 *
 *   Copyright (c) 2008  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: VM-Profiler.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/time.h>
#include <signal.h>
#endif
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "EqHashTable.h"
#include "VM.h"
#include "ErrorProcedures.h"
#include "TextualOutputPort.h"
#include "StringProcedures.h"
#include "Callable.h"
#ifdef _WIN32
#include "OSCompatThread.h"
#endif

using namespace scheme;

#ifdef ENABLE_PROFILER
extern VM* theVM;
extern Object stringTosymbolEx(Object args);
const int VM::SAMPLE_NUM = 50000;

extern  void signal_handler(int signo);

#ifdef _WIN32
static void*
profiler_interrupt_thread(void* p){
    LARGE_INTEGER duetime;
    HANDLE hTimer = NULL;
    VM* vm = reinterpret_cast<VM *>(p);
    // Create new timer(no named, names must be unique)
    hTimer = CreateWaitableTimer(NULL, FALSE, NULL);

    // duetime = 0;
    duetime.QuadPart = -100000LL; // wait 10ms

    SetWaitableTimer(hTimer, &duetime, 0, NULL, NULL, 0);
    for(;;){
        if(WaitForSingleObject(hTimer, INFINITE) != WAIT_OBJECT_0){
            // FAIL
            return NULL;
        }else{
            EnterCriticalSection(&vm->profilerCs_);
            if(vm->profilerTerminate_){
                LeaveCriticalSection(&vm->profilerCs_);
                return NULL;
            }
            if(vm->profilerEnable_){
                SuspendThread(vm->vmThread_);
                vm->collectProfile();
                ResumeThread(vm->vmThread_);
            }
            LeaveCriticalSection(&vm->profilerCs_);
            SetWaitableTimer(hTimer, &duetime, 0, NULL, NULL, 0);
        }
    }
}
#endif

void VM::initProfiler()
{
    samples_ = Object::makeObjectArray(SAMPLE_NUM);
    callSamples_ = Object::makeObjectArray(SAMPLE_NUM);
    callHash_ = Object::makeEqHashTable();
    totalSampleCount_ = 0;
    for (int i = 0; i < SAMPLE_NUM; i++) {
        samples_[i] = Object::Nil;
        callSamples_[i] = Object::Nil;
    }
#ifdef _WIN32
    const VM* target = this;
    // FIXME: May leak thread handle
    vmThread_ = OpenThread(THREAD_SUSPEND_RESUME,FALSE,GetCurrentThreadId());
    profilerEnable_ = false;
    profilerTerminate_ = false;
    profilerInterruptThread_ = new Thread();
    InitializeCriticalSection(&profilerCs_);
    // FIXME: Move to startTimer??
    profilerInterruptThread_->create(profiler_interrupt_thread, (void*)target, 
                                     Thread::priorityNormal,
                                     "Profiler Interrupt");
#else
    // UNIX SIGPROF sampler
    struct sigaction act;
    act.sa_handler = &signal_handler; // set signal_handler
    act.sa_flags = SA_RESTART;        // restart system call after signal handler

    if (sigaction(SIGPROF, &act, NULL) != 0) {
        callAssertionViolationImmidiaImmediately(this, "profiler", "sigaction failed");
    }
#endif
    startTimer();
}

void VM::stopProfiler()
{
#ifdef _WIN32
    EnterCriticalSection(&profilerCs_);
    profilerTerminate_ = true;
    LeaveCriticalSection(&profilerCs_);
#endif
    stopTimer();
}

void VM::startTimer()
{
    profilerRunning_ = true;
#ifdef _WIN32
    EnterCriticalSection(&profilerCs_);
    profilerEnable_ = true;
    LeaveCriticalSection(&profilerCs_);
#else
    // UNIX SIGPROF sampler
    const int INTERVAL_USEC = 10 * 1000;
    struct itimerval tval, oval;
    tval.it_interval.tv_sec = 0;
    tval.it_interval.tv_usec = INTERVAL_USEC;
    tval.it_value.tv_sec = 0;
    tval.it_value.tv_usec = INTERVAL_USEC;
    setitimer(ITIMER_PROF, &tval, &oval);
#endif
}

void VM::stopTimer()
{
    profilerRunning_ = false;
#ifdef _WIN32
    EnterCriticalSection(&profilerCs_);
    profilerEnable_ = false;
    LeaveCriticalSection(&profilerCs_);
#else
    // UNIX SIGPROF sampler
    struct itimerval tval, oval;
    tval.it_interval.tv_sec = 0;
    tval.it_interval.tv_usec = 0;
    tval.it_value.tv_sec = 0;
    tval.it_value.tv_usec = 0;
    setitimer(ITIMER_PROF, &tval, &oval);
#endif
}

void VM::collectProfile()
{
    static int i = 0;
    if (!profilerRunning_) return;
    if (i >= SAMPLE_NUM) {
        currentErrorPort_.toTextualOutputPort()->display(this, UC("buffer full profiler stopped."));
        stopTimer();
    } else if ((*pc_).val == labelReturn_ && ac_.isCProcedure()) {
        samples_[i++] = ac_;
    } else {
        samples_[i++] = cl_;
    }
    totalSampleCount_++;
}

void VM::storeCallSample()
{
    EqHashTable* callHash = callHash_.toEqHashTable();
    for (int i = 0; i < SAMPLE_NUM; i++) {
        const Object proc = callSamples_[i];
        if (proc.isNil()) continue;
        const Object count = callHash->ref(proc, Object::False);
        if (count.isNil()) {
            /* */
        } else if (count.isFixnum()) {
            callHash->set(proc, Object::makeFixnum(count.toFixnum() + 1));
        } else {
            callHash->set(proc, Object::makeFixnum(1));
        }
        callSamples_[i] = Object::Nil;
    }
}


Object VM::getProfileResult()
{
    profilerRunning_ = false;
    stopProfiler();
    if(!samples_){
        return Object::Nil;
    }
    Object ret = Object::Nil;
    for (int i = 0; i < SAMPLE_NUM; i++) {
        const Object o = samples_[i];
        if (o.isProcedure()) {
            ret = Pair::append2(ret, L1(o));
        }
    }

    storeCallSample();
    return Object::cons(Object::makeFixnum(totalSampleCount_), Object::cons(callHash_, ret));
}

// this is slow, because it creates namespace hash for each call.
Object VM::getClosureName(Object closure)
{
//    EqHashTable* nameSpace = nameSpace_.toEqHashTable()->swap().toEqHashTable();
    if (closure.isCProcedure()) {
        return getCProcedureName(closure);
    } else if (closure.isCallable()) {
        return closure.toCallable()->toString();
    } else if (closure.isClosure()) {
        return "my-todo";
//         const Object name = nameSpace->ref(closure, notFound_);
//         if (name == notFound_) {
//             return Object::False;
//         } else {
//             return stringTosymbol(name.cdr());
//         }
    } else {
        return Object::False;
    }
}
#else
Object VM::getClosureName(Object closure)
{
    return Object::False;
}


#endif // ENABLE_PROFILER
