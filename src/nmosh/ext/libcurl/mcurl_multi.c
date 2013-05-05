#include <curl/curl.h>

void*
mcurl_multi_init(void){
    return curl_multi_init();
}
