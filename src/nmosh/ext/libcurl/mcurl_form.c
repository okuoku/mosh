#include <curl/curl.h>
#include <nmosh/plugin-if.h>

MOSHEXPORT
void
mcurl_formfree(void* form){
    curl_formfree(form);
}

MOSHEXPORT
int
mcurl_formadd_cstring(void* start, void* end, 
                      const char* name, const char* data){
    return
        curl_formadd(start,end,
                     CURLFORM_COPYNAME, name,
                     CURLFORM_COPYCONTENTS, data,
                     CURLFORM_END);
}

MOSHEXPORT
int
mcurl_formadd_blob(void* start, void* end,
                   const char* name, void* ent, int len,
                   const char* file_name, const char* content_type){
    if(file_name){
        /* nb: file_name is mandatory if the content_type was provided */
        return
            curl_formadd(start,end,
                         CURLFORM_COPYNAME, name,
                         CURLFORM_FILE, file_name,
                         CURLFORM_CONTENTTYPE, content_type,
                         CURLFORM_CONTENTSLENGTH, len,
                         CURLFORM_COPYCONTENTS, ent,
                         CURLFORM_END);
    }else{
        /* This one is ordinal post entry */
        return
            curl_formadd(start,end,
                         CURLFORM_COPYNAME, name,
                         CURLFORM_CONTENTSLENGTH, len,
                         CURLFORM_COPYCONTENTS, ent,
                         CURLFORM_END);
    }
}

NMOSH_CONSTANT_BEGIN(mcurl_form)
    /* FIXME: Export errors here */
NMOSH_CONSTANT_END()
