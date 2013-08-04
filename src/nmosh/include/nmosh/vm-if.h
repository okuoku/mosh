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
#ifndef NMOSHDLL
#define NMOSHDLL 
#endif

/* Object types */
typedef enum{
    NMOSH_OBJECT_INVALID,
    NMOSH_OBJECT_NIL,
    NMOSH_OBJECT_TRUE,
    NMOSH_OBJECT_FALSE,
    NMOSH_OBJECT_PAIR,
    NMOSH_OBJECT_VECTOR, /* FIXME: Implement it */
    NMOSH_OBJECT_INTEGER,
    NMOSH_OBJECT_RATNUM, /* FIXME: Implement it */
    NMOSH_OBJECT_STRING, /* FIXME: Implement it */
    NMOSH_OBJECT_BYTEVECTOR,
    NMOSH_OBJECT_FLONUM,
}nmosh_object_type;

/* Error code */
#define NMOSH_SUCCESS 0
#define NMOSH_TYPE_VIOLATION 1

/* Nmosh VM Context. */
typedef struct {
    /* FIXME: Move it to private header */
    void* vm; /* Pointer to VM */
    void* library_load; /* (library-load name) */
    void* library_lookup; /* (library-lookup name symname) */
}nmosh_vm_s;

typedef struct {
    /* VM params */
    int verbose;
    char* loadpath;
    /* Heap */
    /* Scheme frontend */
}nmosh_vmattr_s;


/* Type of nmosh VM instance */
typedef nmosh_vm_s* nmosh_vm_t;

/* Type of nmosh VM configuration */
typedef nmosh_vmattr_s* nmosh_vmattr_t;

/* Pointer to object (GC guarded ) */
typedef void* nmosh_object_t;
/* Object cursor *NOT* GC guarded */
typedef void* nmosh_object_cursor_t;

/* Exports */

/*   OBJECT  */
NMOSHDLL void nmosh_object_destroy(nmosh_object_t obj);
NMOSHDLL int nmosh_object_cursor(nmosh_object_t obj,
                                 nmosh_object_cursor_t* out);
NMOSHDLL int nmosh_object_export(const nmosh_export_entry_t* e, 
        nmosh_object_t* out);
NMOSHDLL int nmosh_object_export_gc(const nmosh_export_entry_t* e, 
        nmosh_object_t* out);
NMOSHDLL void* nmosh_rootset_alloc(size_t size);
NMOSHDLL void nmosh_rootset_free(void* p);

/*   CURSOR */
NMOSHDLL nmosh_object_type nmosh_cursor_type(nmosh_object_cursor_t in);
NMOSHDLL int nmosh_cursor_car(nmosh_object_cursor_t in,
                              nmosh_object_cursor_t* out);
NMOSHDLL int nmosh_cursor_cdr(nmosh_object_cursor_t in,
                              nmosh_object_cursor_t* out);
NMOSHDLL int nmosh_cursor_bytevector_size(nmosh_object_cursor_t in,
                                          uintptr_t* out);
NMOSHDLL int nmosh_cursor_bytevector(nmosh_object_cursor_t in,
                                     uintptr_t offset, uintptr_t count,
                                     void* buf);
NMOSHDLL int nmosh_cursor_bytevector_pointer(nmosh_object_cursor_t in,
        void** out);
NMOSHDLL int nmosh_cursor_integer_unsigned(nmosh_object_cursor_t in,
                                           uint64_t* out);
NMOSHDLL int nmosh_cursor_integer_signed(nmosh_object_cursor_t in,
                                         int64_t* out);
NMOSHDLL int nmosh_cursor_pointer(nmosh_object_cursor_t in, 
        void** out);
NMOSHDLL int nmosh_cursor_double(nmosh_object_cursor_t in,
                                 double* out);

/*   LIBRARY */
NMOSHDLL int nmosh_library_lookup(nmosh_vm_t vm, const char* libname,
                                  const char* symbol,
                                  nmosh_object_t* out_object);
/*   VM */
NMOSHDLL void nmosh_vm_destroy(nmosh_vm_t vm);
NMOSHDLL int nmosh_vm_create(nmosh_vmattr_t attr, nmosh_vm_t* out_vm);
NMOSHDLL int nmosh_vm_apply(nmosh_vm_t vm, nmosh_object_t closure,
                            nmosh_object_t arg, nmosh_object_t* out_obj);
NMOSHDLL int nmosh_vm_set(nmosh_vm_t vm, const char* symname, 
                          nmosh_object_t obj);

/*   VM ATTR */
NMOSHDLL void nmosh_vmattr_destroy(nmosh_vmattr_t attr);
NMOSHDLL int nmosh_vmattr_init(nmosh_vmattr_t* out);
NMOSHDLL int nmosh_vmattr_setverbose(nmosh_vmattr_t attr, int verbose);
NMOSHDLL int nmosh_vmattr_setloadpath(nmosh_vmattr_t attr, const char* loadpath);

/*   Library init */
NMOSHDLL int nmosh_init(void);

NMOSH_COMMON_END

#endif
