#ifndef __NMOSH_C_H
#define __NMOSH_C_H

/* PRIVATE: Internal C binding for Mosh heap/VM */

#ifdef __cplusplus
extern "C" {
#endif
// }

typedef void* (*nmosh_callback_func_t)(void* ctx, void* arg);

void* moshvm_alloc(void);
int moshvm_launch(void* pvm, void* param);
void moshvm_set_value_string(void* pvm, const char* symname, const char* value);
void moshvm_set_value_pointer(void* pvm, const char* symname, void* value);
void moshvm_set_value_boolean(void* pvm, const char* symname, int b);
void* moshvm_export_object(const nmosh_export_entry_t en[]);
uintptr_t moshvm_callback_call(void* p, void* l);
void* moshvm_export_object_map_ptr(nmosh_export_object_map_cb_t cb,
                                   void* ctx, const void** in_ptrptr);
void* moshvm_export_object_map(nmosh_export_object_map_cb_t cb,
                               void* ctx, const void* in_ptr);

// {
#ifdef __cplusplus
}
#endif

#endif
