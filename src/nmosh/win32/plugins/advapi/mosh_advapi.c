#include <nmosh/plugin-if.h>
#include <windows.h>

/* --------------------------------------------------------------
 * Registry
 * -------------------------------------------------------------- */

MOSHEXPORT
int /* Win32 error */
madvapi_reg_open(void* hkey, void* regname, int access, void* out_hkey){
    LONG r;
    r = RegOpenKeyExW(hkey, regname, 0, access, out_hkey);
    return r;
}

MOSHEXPORT
int /* Win32 error */
madvapi_reg_close(void* hkey){
    return RegCloseKey(hkey);
}

MOSHEXPORT
int
madvapi_reg_query(void* hkey, void* regname, void* buf, int* inout_size, 
                  int* out_type){
    return RegQueryValueExW(hkey, regname, 0, out_type, buf, inout_size);
}


NMOSH_CONSTANT_BEGIN(mosh_advapi)
    /* Registry */
    NMOSH_EXPORT_SYMBOL_POINTER_INT(HKEY_CLASSES_ROOT)
    NMOSH_EXPORT_SYMBOL_POINTER_INT(HKEY_CURRENT_CONFIG)
    NMOSH_EXPORT_SYMBOL_POINTER_INT(HKEY_CURRENT_USER)
    NMOSH_EXPORT_SYMBOL_POINTER_INT(HKEY_LOCAL_MACHINE)
    NMOSH_EXPORT_SYMBOL_POINTER_INT(HKEY_USERS)
    NMOSH_EXPORT_SYMBOL_POINTER_INT(HKEY_PERFORMANCE_DATA)
    NMOSH_EXPORT_SYMBOL_INT(KEY_CREATE_LINK)
    NMOSH_EXPORT_SYMBOL_INT(KEY_CREATE_SUB_KEY)
    NMOSH_EXPORT_SYMBOL_INT(KEY_ENUMERATE_SUB_KEYS)
    NMOSH_EXPORT_SYMBOL_INT(KEY_EXECUTE)
    NMOSH_EXPORT_SYMBOL_INT(KEY_NOTIFY)
    NMOSH_EXPORT_SYMBOL_INT(KEY_QUERY_VALUE)
    NMOSH_EXPORT_SYMBOL_INT(KEY_SET_VALUE)
    NMOSH_EXPORT_SYMBOL_INT(KEY_ALL_ACCESS)
    NMOSH_EXPORT_SYMBOL_INT(KEY_READ)
    NMOSH_EXPORT_SYMBOL_INT(KEY_WRITE)
    /* Cred */
    NMOSH_EXPORT_SYMBOL_INT(CRED_MAX_STRING_LENGTH)
    NMOSH_EXPORT_SYMBOL_INT(CRED_MAX_CREDENTIAL_BLOB_SIZE)
    NMOSH_EXPORT_SYMBOL_INT(CRED_PERSIST_SESSION)
    NMOSH_EXPORT_SYMBOL_INT(CRED_PERSIST_LOCAL_MACHINE)
    NMOSH_EXPORT_SYMBOL_INT(CRED_PERSIST_ENTERPRISE)
NMOSH_CONSTANT_END()

NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(mosh_advapi);
