#include <nmosh/plugin-if.h>
#include <android/log.h>
#include "SDL.h"

/* Asset wrappers
 *  Android uses a unique "asset" feature to provide assets to the app.
 *  In Nmosh, SDL binding does not provide entire its RWops abstruction.
 *  because Android is (likely) the only platform that uses ROM FS. */
MOSHEXPORT
int /* 0 = success, err */
msdl_android_asset_open(const char* path, SDL_RWops** out_handle){
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

extern void* nmosh_archive_ptr;
extern uintptr_t nmosh_archive_size;

static void
msdl_android_mosh_init(void){
    int r;
    int size;
    SDL_RWops* f;
    __android_log_print(ANDROID_LOG_DEBUG,"Nmosh","Loading library archive...");
    /* Open and calc size */
    f = SDL_RWFromFile("archive.fasl", "r");
    if(!f){
        __android_log_print(ANDROID_LOG_DEBUG,"Nmosh",
                            "Archive open failed(%s)...",SDL_GetError());
        return;
    }
    r = SDL_RWseek(f, 0, RW_SEEK_END);
    size = SDL_RWtell(f);
    r = SDL_RWseek(f, 0, RW_SEEK_SET);
    /* FIXME: Free it later */
    nmosh_archive_ptr = malloc(size);
    __android_log_print(ANDROID_LOG_DEBUG,"Nmosh","Reading library archive...");
    r = SDL_RWread(f, nmosh_archive_ptr, 1, size);
    nmosh_archive_size = size;
    __android_log_print(ANDROID_LOG_DEBUG,"Nmosh","Library loaded (size = %d, r = %d)",size,r);
}

MOSHEXPORT
int
SDL_main(int ac, char** av){
    int argc;
    const char* argv[5];
    argv[0] = "bogus";
    argv[1] = "--verbose";
    argv[2] = "-T";
    argv[3] = "test-sdl";
    argv[4] = NULL;
    argc = 4;
    msdl_android_mosh_init();
    __android_log_print(ANDROID_LOG_DEBUG,"Nmosh","Launch mosh_main");
    mosh_main(argc,argv);
    __android_log_print(ANDROID_LOG_DEBUG,"Nmosh","Exit...");
    return 0;
}
