#include <curl/curl.h>
#include <nmosh/plugin-if.h>

MOSHEXPORT
int
mcurl_global_init(int flags){
    /* FIXME: flags is long */
    return curl_global_init(flags);
}

MOSHEXPORT
void
mcurl_global_cleanup(void){
    curl_global_cleanup();
}

NMOSH_CONSTANT_BEGIN(mcurl_global)
    NMOSH_EXPORT_SYMBOL_INT(CURL_GLOBAL_ALL)
    NMOSH_EXPORT_SYMBOL_INT(CURL_GLOBAL_SSL)
    NMOSH_EXPORT_SYMBOL_INT(CURL_GLOBAL_WIN32)
    NMOSH_EXPORT_SYMBOL_INT(CURL_GLOBAL_NOTHING)
    NMOSH_EXPORT_SYMBOL_INT(CURL_GLOBAL_DEFAULT)
    NMOSH_EXPORT_SYMBOL_INT(CURL_GLOBAL_ACK_EINTR)
NMOSH_CONSTANT_END()
