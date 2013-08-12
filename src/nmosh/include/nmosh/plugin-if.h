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

#define NMOSH_PLUGIN_ABI_VERSION 1


typedef struct nmosh_plugin_callback_table_s nmosh_plugin_callback_table_t;

typedef void (*nmosh_plugin_callback_fill_t)(nmosh_plugin_callback_table_t**
                                             tbl, 
                                             const char* plugin_name,
                                             uintptr_t abi);

// #define NMOSH_EXPORT_OBJECT_SKIP ((void*)(intptr_t)-1)

typedef void* (*nmosh_export_object_map_cb_t)
    (void* ctx, const void* in_ptr);
typedef void* (*nmosh_export_object_map_ptr_t)
    (nmosh_export_object_map_cb_t cb, void* ctx,
     const void** in_ptrptr);
typedef void* (*nmosh_export_object_map_t)
    (nmosh_export_object_map_cb_t cb, void* ctx,
     const void* in_ptr);

struct nmosh_plugin_callback_table_s {
    uintptr_t vm_private;
    uintptr_t abi_version;
    nmosh_export_callback_t       cb_export;
    nmosh_callback_call_t         cb_call;
    nmosh_export_object_map_ptr_t cb_export_map_ptr;
    nmosh_export_object_map_t     cb_export_map;
};

#define NMOSH_PLUGIN_DEFINE(name) \
NMOSH_CONSTANT_BEGIN(name) \
NMOSH_CONSTANT_END() \
NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(name)

#ifdef NMOSHPLUGIN_EMBED
/* FIXME: Use declspec(dllimport) ?? */
#include "nmosh-c.h"
#define nmosh_callback_call moshvm_callback_call
#define nmosh_export_callback moshvm_export_object
#define nmosh_export_object_map moshvm_export_object_map
#define nmosh_export_object_map_ptr moshvm_export_object_map_ptr
#define NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(name) \
    void nmosh_plugin_init_##name(nmosh_plugin_callback_fill_t bogus, \
                                  void** objout){ \
        (void)bogus; \
        if(NMOSH_CONSTANT_NAME(name)){ \
            *objout = NMOSH_EXPORT(NMOSH_CONSTANT_NAME(name)); \
        }else{ \
            *objout = 0; \
        }}
#else

#define nmosh_callback_call nmosh_plugin_callback_table->cb_call
#define nmosh_export_callback nmosh_plugin_callback_table->cb_export
#define nmosh_export_object_map_ptr \
    nmosh_plugin_callback_table->cb_export_map_ptr
#define nmosh_export_object_map \
    nmosh_plugin_callback_table->cb_export_map

#define NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(name) \
    nmosh_plugin_callback_table_t* nmosh_plugin_callback_table; \
    MOSHEXPORT void nmosh_plugin_init_##name(nmosh_plugin_callback_fill_t \
                                             fill, \
                                             void** objout){ \
        fill(&nmosh_plugin_callback_table, #name, NMOSH_PLUGIN_ABI_VERSION); \
        if(NMOSH_CONSTANT_NAME(name)){\
            *objout = NMOSH_EXPORT(NMOSH_CONSTANT_NAME(name)); \
        }else{ \
            *objout = 0; \
        }}
extern nmosh_plugin_callback_table_t* nmosh_plugin_callback_table;
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
#define NMOSH_EXPORT_TYPE_OBJECT  7 /* arg0 = Ptr */
#define NMOSH_EXPORT_TYPE_UINT    8 /* arg0 = Value */

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
      (uintptr_t)(intptr_t)name, \
      0 } ,

#define NMOSH_EXPORT_SYMBOL_UINT(name) \
    { NMOSH_EXPORT_TYPE_UINT, \
      #name, \
      (uintptr_t)name, \
      0 } ,

#define NMOSH_EXPORT_RAW(str,val) \
    { NMOSH_EXPORT_TYPE_UINT, \
      str, \
      (uintptr_t)val, \
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

#define NMOSH_EXPORT_SYMBOL_OBJECT(name, ptr) \
    { NMOSH_EXPORT_TYPE_OBJECT , \
      name, \
      ptr, \
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

#define NMOSH_EXPORT_MAP(cb, ctx, ptr) \
        nmosh_export_object_map(cb, ctx, ptr)

#define NMOSH_EXPORT_MAP_POINTER(cb, ctx, ptr) \
        nmosh_export_object_map_ptr(cb, ctx, ptr)

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
