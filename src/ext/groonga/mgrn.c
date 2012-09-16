#include "config.h"
#include <stdint.h>
#include <stdlib.h>
#include <groonga.h>

typedef void (*nmosh_callback_t)(void* ticket, uintptr_t out0,int32_t out1);

static void
grn_callback(grn_ctx *ctx, int flags, void* p){
    nmosh_callback_t cb;
    grn_obj *buf;
    grn_ctx_info info;
    uint32_t len;
    void* out;
    if(ctx && (flags & GRN_CTX_TAIL)){
        grn_ctx_info_get(ctx, &info);
        buf = info.outbuf;
        /* FIXME: Handle error here! */
        len = GRN_TEXT_LEN(buf);
        out = malloc(len);
        memcpy(out,GRN_TEXT_VALUE(buf),len);
        cb = (nmosh_callback_t)GRN_CTX_USER_DATA(ctx)->ptr;
        cb(p, (uintptr_t)out,len); // Chime
        GRN_BULK_REWIND(buf);
    }
}

MOSHEXPORT
void
mgrn_init(void){
    grn_init();
}

MOSHEXPORT
void
mgrn_request(void* p,void* str, int len){
    grn_ctx* ctx = (grn_ctx *)p;
    grn_ctx_send(ctx, str, len, 0);
}

MOSHEXPORT
void
mgrn_result_free(void* p){
    free(p);
}

MOSHEXPORT
void*
mgrn_create(void* callback,void* ticket){
    grn_ctx* c;
    c = grn_ctx_open(GRN_CTX_USE_QL);
    GRN_CTX_USER_DATA(c)->ptr = callback;
    grn_ctx_recv_handler_set(c, &grn_callback, ticket);
    return c;
}

MOSHEXPORT
void*
mgrn_db_create_local(void* p, const char* path){
    grn_ctx* ctx = (grn_ctx *)p;
    return grn_db_create(ctx, path, NULL);
}

MOSHEXPORT
void*
mgrn_db_open_local(void* p,const char* path){
    grn_ctx* ctx = (grn_ctx *)p;
    return grn_db_open(ctx, path);
}

MOSHEXPORT
void
mgrn_dispose(void* p){
    grn_ctx_close((grn_ctx *)p);
}
