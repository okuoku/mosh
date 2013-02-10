#ifndef __NMOSH_VM_IF_H
#define __NMOSH_VM_IF_H

#include <nmosh/common.h>
#undef NMOSHPLUGIN_EMBED // FIXME: Too hacky
#include <nmosh/plugin-if.h>

NMOSH_COMMON_BEGIN

#ifdef NMOSH_BUILD_MODULE
#if defined(_WIN32)
#define NMOSHDLL __declspec(dllexport)
#endif
#else
#if defined(_WIN32)
#define NMOSHDLL __declspec(dllimport)
#endif
#endif

/* Error code */
#define NMOSH_SUCCESS 0

/* Nmosh VM Context. */
typedef struct {
    /* FIXME: Move it to private header */
    void* vm; /* Pointer to VM */
    void* library_load; /* (library-load name) */
    void* library_lookup; /* (library-lookup name symname) */
}nmosh_vm_s;

typedef struct {
    int __dummy;
    /* Heap */
    /* Scheme frontend */
}nmosh_vmattr_s;


/* Type of nmosh VM instance */
typedef nmosh_vm_s* nmosh_vm_t;

/* Type of nmosh VM configuration */
typedef nmosh_vmattr_s* nmosh_vmattr_t;

/* Pointer to object (GC guarded ) */
typedef void* nmosh_object_t;

/* Exports */

/*   OBJECT  */
NMOSHDLL void nmosh_object_destroy(nmosh_object_t obj);

/*   LIBRARY */
NMOSHDLL int nmosh_library_lookup(nmosh_vm_t vm, const char* libname,
                                  const char* symbol,
                                  nmosh_object_t* out_object);
/*   VM */
NMOSHDLL void nmosh_vm_destroy(nmosh_vm_t vm);
NMOSHDLL int nmosh_vm_create(nmosh_vmattr_t* attr, nmosh_vm_t* out_vm);
NMOSHDLL int nmosh_vm_apply(nmosh_vm_t vm, nmosh_object_t closure,
                            nmosh_object_t arg, nmosh_object_t* out_obj);

/*   Library init */
NMOSHDLL int nmosh_init(void);

NMOSH_COMMON_END

#endif
