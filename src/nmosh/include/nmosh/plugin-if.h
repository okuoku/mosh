#ifndef __NMOSH__PLUGIN_IF_H
#define __NMOSH__PLUGIN_IF_H

#include <stdint.h>
#include <nmosh/common.h>

NMOSH_COMMON_BEGIN

/* Definitions */
#if !defined(NMOSHPLUGIN_EMBED)
#if defined(_WIN32)
#define MOSHEXPORT __declspec(dllexport)
#else
/* FIXME: Say something about visibility */
#define MOSHEXPORT
#endif
#else
#define MOSHEXPORT /* non export */
#endif /* !NMOSHPLUGIN_EMBED */

/*   Plugin globals */

/* nmosh_export_entry_t: Packet format for object export */
typedef struct {
    int type;
    const char* name;
    uintptr_t arg0;
    uintptr_t arg1;
}nmosh_export_entry_t;
typedef void* (*nmosh_export_callback_t)(const nmosh_export_entry_t* obj);
typedef uintptr_t (*nmosh_callback_call_t)(void*,void*);

#define NMOSH_PLUGIN_DEFINE(name) \
NMOSH_CONSTANT_BEGIN(name) \
NMOSH_CONSTANT_END() \
NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(name)

#ifdef NMOSHPLUGIN_EMBED
/* FIXME: Use declspec(dllimport) ?? */
#include "nmosh-c.h"
#define nmosh_callback_call moshvm_callback_call
#define nmosh_export_callback moshvm_export_object
#define NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(name) \
    void nmosh_plugin_init_##name(void* exp, void* call,void** objout){ \
        if(NMOSH_CONSTANT_NAME(name)){ \
            *objout = NMOSH_EXPORT(NMOSH_CONSTANT_NAME(name)); \
        }else{ \
            *objout = 0; \
        }}
#else

#define NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(name) \
    nmosh_callback_call_t nmosh_callback_call; \
    nmosh_export_callback_t nmosh_export_callback; \
    MOSHEXPORT void nmosh_plugin_init_##name(void* exp, void* call, \
                                             void** objout){ \
        nmosh_export_callback = (nmosh_export_callback_t)exp; \
        nmosh_callback_call = (nmosh_callback_call_t)call; \
        if(NMOSH_CONSTANT_NAME(name)){\
            *objout = NMOSH_EXPORT(NMOSH_CONSTANT_NAME(name)); \
        }else{ \
            *objout = 0; \
        }}

extern nmosh_export_callback_t nmosh_export_callback;
extern nmosh_callback_call_t nmosh_callback_call;
#endif

#define NMOSH_CONSTANT_NAME(name) nmosh_constant_##name
#define NMOSH_PLUGIN_DECLARE(name) \
    extern nmosh_export_entry_t NMOSH_CONSTANT_NAME(name)[]

/*   Export/Import */
/*    ID code */
#define NMOSH_EXPORT_TYPE_TERM    0
#define NMOSH_EXPORT_TYPE_INT     1 /* arg0 = Value */
#define NMOSH_EXPORT_TYPE_DOUBLE  2 /* arg0 = Ptr, arg1 = 0(double) */
#define NMOSH_EXPORT_TYPE_CSTRING 3 /* arg0 = Ptr */
#define NMOSH_EXPORT_TYPE_BUFFER  4 /* arg0 = Ptr, arg1 = length */
#define NMOSH_EXPORT_TYPE_STRUCT  5 /* arg0 = Entry */
#define NMOSH_EXPORT_TYPE_POINTER 6 /* arg0 = Ptr */

#define NMOSH_EXPORT_BEGIN(name) \
    const nmosh_export_entry_t name[] = {

#define NMOSH_EXPORT_END() \
        { NMOSH_EXPORT_TYPE_TERM, 0, 0, 0 } \
    };

/* FIXME: WHY can't we use 'const' here in Win32?? */
#define NMOSH_CONSTANT_BEGIN(name) \
    nmosh_export_entry_t NMOSH_CONSTANT_NAME(name)[] = {

#define NMOSH_CONSTANT_END() NMOSH_EXPORT_END()

#define NMOSH_EXPORT_INT(name, val) \
    { NMOSH_EXPORT_TYPE_INT, \
      name, \
      val, \
      0 } ,

#define NMOSH_EXPORT_POINTER(name, val) \
    { NMOSH_EXPORT_TYPE_POINTER, \
      name, \
      (uintptr_t)val, \
      0 } ,

#define NMOSH_EXPORT_SYMBOL_INT(name) \
    { NMOSH_EXPORT_TYPE_INT, \
      #name, \
      (uintptr_t)name, \
      0 } ,

#define NMOSH_EXPORT_FLOAT(name, val) \
    { NMOSH_EXPORT_TYPE_FLOAT, \
      name, \
      (uintptr_t)&val, \
      0 } ,

#define NMOSH_EXPORT_CSTRING(name, val) \
    { NMOSH_EXPORT_TYPE_CSTRING, \
      name, \
      (uintptr_t)val, \
      0 } ,

#define NMOSH_EXPORT_BUFFER(name, val, size) \
    { NMOSH_EXPORT_TYPE_BUFFER, \
      name, \
      (uintptr_t)val, \
      size } ,

#define NMOSH_EXPORT_SYMBOL_POINTER(name) \
    { NMOSH_EXPORT_TYPE_POINTER , \
      #name, \
      (uintptr_t)&name, \
      0 } ,

#define NMOSH_EXPORT_SYMBOL_POINTER_INT(name) \
    { NMOSH_EXPORT_TYPE_POINTER , \
      #name, \
      (uintptr_t)name, \
      0 } ,

#define NMOSH_EXPORT_SYMBOL_PLUGIN(name) \
    { NMOSH_EXPORT_TYPE_STRUCT , \
      #name, \
      (uintptr_t)&NMOSH_CONSTANT_NAME(name), \
      0 } ,

#define NMOSH_EXPORT(name) \
    nmosh_export_callback(name)

#define NMOSH_APPLY(closure, data) \
    nmosh_callback_call(closure, data)

typedef void (*nmosh_chime_callback_t)(void* chime_param);

typedef struct {
    nmosh_chime_callback_t caller;
    void* state;
}nmosh_chime_call_t;

typedef struct {
    void* vm;
    void* closure;
}nmosh_vm_callback_t;

NMOSH_COMMON_END

#endif /* __NMOSH__PLUGIN_IF_H */
