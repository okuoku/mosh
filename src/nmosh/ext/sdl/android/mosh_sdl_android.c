#include <nmosh/plugin-if.h>
#include "SDL.h"

/* Asset wrappers
 *  Android uses a unique "asset" feature to provide assets to the app.
 *  In Nmosh, SDL binding does not provide entire its RWops abstruction.
 *  because Android is (likely) the only platform that uses ROM FS. */
MOSHEXPORT
int /* 0 = success, err */
msdl_andorid_asset_open(const char* path, SDL_RWops** out_handle){
    SDL_RWops* r;
    r = SDL_RWFromFile(path, "r");
    if(r){
        *out_handle = r;
        return 0;
    }else{
        return -1;
    }
}

MOSHEXPORT
int
msdl_android_asset_tell(SDL_RWops* handle){
    return SDL_RWtell(handle);
}

#define MSDL_WHENCE_SET 0
#define MSDL_WHENCE_CUR 1
#define MSDL_WHENCE_END 2
MOSHEXPORT
int
msdl_android_asset_seek(SDL_RWops* handle, int offset, int whence){
    int sdlwhence = 999999; /* Some invalid value */
    int r;
    switch(whence){
        case MSDL_WHENCE_SET:
            sdlwhence = RW_SEEK_SET;
            break;
        case MSDL_WHENCE_CUR:
            sdlwhence = RW_SEEK_CUR;
            break;
        case MSDL_WHENCE_END:
            sdlwhence = RW_SEEK_END;
            break;
    }
    r = SDL_RWseek(handle,offset,sdlwhence);
    return r;
}

MOSHEXPORT
void
msdl_android_asset_close(SDL_RWops* handle){
    /* We never Write through SDL_RW*, so it is safe to ignore error here. 
     * SDL_RWclose may return error when any error occured during flushing its
     * cache. */
    (void)SDL_RWclose(handle);
}

MOSHEXPORT
int
msdl_android_asset_read(SDL_RWops* handle, void* buf, int size){
    int r;
    r = SDL_RWread(handle, buf, 1, size);
    return r;
}
