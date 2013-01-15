/*
 * OSCompatThread.cpp - thread interfaces.
 *
 *   Copyright (c) 2009  Higepon(Taro Minowa)  <higepon@users.sourceforge.jp>
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
 *  $Id: OSCompatThread.cpp 183 2008-07-04 06:19:28Z higepon $
 */

#include "scheme.h"
#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "UTF8Codec.h"
#include "Transcoder.h"
#include "OSCompat.h"
#include "OSCompatThread.h"
#include "gc.h"

using namespace scheme;

#ifndef MONA
ThreadSpecificKey* Thread::selfKey;

#ifdef _WIN32
// Took from : http://msdn.microsoft.com/en-us/library/xcb2z8hs.aspx

const DWORD MS_VC_EXCEPTION=0x406D1388;

#ifdef _MSC_VER
#pragma pack(push,8)
#endif
typedef struct tagTHREADNAME_INFO
{
   DWORD dwType; // Must be 0x1000.
   LPCSTR szName; // Pointer to name (in user addr space).
   DWORD dwThreadID; // Thread ID (-1=caller thread).
   DWORD dwFlags; // Reserved for future use, must be zero.
} THREADNAME_INFO;
#ifdef _MSC_VER
#pragma pack(pop)
#endif

static void SetThreadName( DWORD dwThreadID, const char* threadName)
{
   THREADNAME_INFO info;
   info.dwType = 0x1000;
   info.szName = threadName;
   info.dwThreadID = dwThreadID;
   info.dwFlags = 0;

   __try
   {
      RaiseException( MS_VC_EXCEPTION, 0, sizeof(info)/sizeof(ULONG_PTR), (ULONG_PTR*)&info );
   }
   __except(EXCEPTION_EXECUTE_HANDLER)
   {
   }
}
#endif

static void
thread_setmyname(const char* name){
#if defined(_WIN32)
    // Defined in above
    // name will be copied to VC runtime internal
    SetThreadName(-1 /* Myself */, name);
#endif
    // Do nothing on unsupported platform
}

typedef void (*nmosh_ticket_callback_t)(void* ticket, void* arg);

#ifdef _WIN32
static unsigned int __stdcall stubFunction(void* param)
#else
static void* stubFunction(void* param)
#endif
{
    Thread::StubInfo* info = (Thread::StubInfo*)param;
    if (!Thread::setSpecific(info->selfKey, info->thread)) {
        fprintf(stderr, "fatal : Thread store self\n");
        exit(-1);
    }
    if(info->name){
        thread_setmyname(info->name);
    }
    info->returnValue = info->func(info->argument);
    if(info->ticketFunction){
        // FIXME: Detach the thread??
        const nmosh_ticket_callback_t stubcallback = 
            (const nmosh_ticket_callback_t)info->ticketFunction;
        stubcallback(info->ticketData, info->returnValue);
    }
#ifdef _WIN32
    return (unsigned int)(uintptr_t)info->returnValue;
#else
    return info->returnValue;
#endif
}

bool Thread::create(void* (*start)(void*), void* arg, ThreadPriority prio,
                    const char* threadname, void* ticket_func,
                    void* ticket_data)
{
    stubInfo_ = new StubInfo;
    stubInfo_->func = start;
    stubInfo_->argument = arg;
    stubInfo_->thread = this;
    stubInfo_->selfKey = selfKey;
    stubInfo_->priority = prio;
    stubInfo_->name = threadname;
    stubInfo_->ticketFunction = ticket_func;
    stubInfo_->ticketData = ticket_data;
#ifdef _WIN32
    unsigned int threadId;
    thread_ = (HANDLE)GC_beginthreadex(0, 0, stubFunction,stubInfo_, 0, &threadId);
    return thread_ != 0;
#else
    if (GC_pthread_create(&thread_, NULL, stubFunction , stubInfo_) == 0) {
        return true;
    } else {
        setLastError();
        return false;
    }
#endif
}
#endif
