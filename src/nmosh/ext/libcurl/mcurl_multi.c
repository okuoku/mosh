#include <nmosh/plugin-if.h>
#include <curl/curl.h>


void*
mcurl_multi_init(void){
    return curl_multi_init();
}

NMOSH_CONSTANT_BEGIN(mcurl_multi)

NMOSH_CONSTANT_END()
