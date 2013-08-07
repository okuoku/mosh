#include <nmosh/plugin-if.h>
#include <windows.h>
#include <wincred.h>

/* --------------------------------------------------------------
 * Credential management
 * -------------------------------------------------------------- */

/* NB: Max blob size is badly limited(512 bytes)! */

MOSHEXPORT
void
madvapi_credfree(void* pcred){
    CredFree(pcred);
}

MOSHEXPORT
void
madvapi_credblob(void* p, void** out_blob, int* out_size){
	PCREDENTIALW pcred = (PCREDENTIALW)p;
    *out_blob = pcred->CredentialBlob;
    *out_size = pcred->CredentialBlobSize;
}

MOSHEXPORT
int /* Win32 error */
madvapi_credread_generic(void* targetname, void** out_cred){
    CREDENTIALW* pcred;
    BOOL b;
    b = CredReadW((LPCWSTR)targetname, CRED_TYPE_GENERIC, 0, &pcred);
    if(!b){
        return GetLastError();
    }
	*out_cred = pcred;
    return 0;
}

MOSHEXPORT
int /* Win32 error */
madvapi_credwrite_generic(void* targetname, void* comment,
                          void* blob, int size, int persist){
    CREDENTIALW cred;
    BOOL b;
    ZeroMemory(&cred,sizeof(cred));
    cred.Flags = 0;
    cred.Type = CRED_TYPE_GENERIC;
    cred.TargetName = (LPWSTR)targetname;
    cred.Comment = (LPWSTR)comment;
    cred.CredentialBlobSize = size;
    cred.CredentialBlob = (LPBYTE)blob;
    cred.Persist = persist;
    cred.AttributeCount = 0;
    b = CredWriteW(&cred, 0);
    if(!b){
        return GetLastError();
    }
    return 0;
}

NMOSH_CONSTANT_BEGIN(mosh_wincrypt)
    /* Cred */
    NMOSH_EXPORT_SYMBOL_INT(CRED_MAX_STRING_LENGTH)
    NMOSH_EXPORT_SYMBOL_INT(CRED_MAX_CREDENTIAL_BLOB_SIZE)
    NMOSH_EXPORT_SYMBOL_INT(CRED_PERSIST_SESSION)
    NMOSH_EXPORT_SYMBOL_INT(CRED_PERSIST_LOCAL_MACHINE)
    NMOSH_EXPORT_SYMBOL_INT(CRED_PERSIST_ENTERPRISE)
NMOSH_CONSTANT_END()

NMOSH_PLUGIN_DEFINE_WITH_CONSTANTS(mosh_wincrypt);
