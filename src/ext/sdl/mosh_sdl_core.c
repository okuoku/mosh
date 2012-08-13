/* SDL mainloop and Window handler */

#include "config.h"
#include <stdlib.h>
#include "SDL.h"

#define MSDL_KEYEV_DOWN 1
#define MSDL_KEYEV_UP 2
#define MSDL_KEY_SHIFT 1
#define MSDL_KEY_CTRL 2
#define MSDL_KEY_ALT 3
#define MSDL_KEY_META 4
MOSHEXPORT
int /* 0 for unknown event */
msdl_event_read_key_code(void* ev, int* window_id,int* event, int* code){
    return 0;
}

MOSHEXPORT
int /* 0 for unknown event */
msdl_event_read_key_char(void* ev, int* window_id, int* c){
    return 0;
}

#define MSDL_EV_POINT_CLASS_MOUSE 1
#define MSDL_MOUSEEV_UP 1
#define MSDL_MOUSEEV_DOWN 2
#define MSDL_MOUSEEV_MOTION 3
MOSHEXPORT
int /* 0 for unknown event */
msdl_event_read_pointing(void* p, int* class, 
                         int* window_id,
                         uintptr_t* id, int* action,
                         int* x, int* y, int* delta_x, int* delta_y){
    SDL_Event *ev = (SDL_Event *)p;
    switch(ev->type){
        case SDL_MOUSEWHEEL:
            *window_id = ev->wheel.windowID;
            *delta_x = ev->wheel.x;
            *delta_y = ev->wheel.y;
            return 1;
        case SDL_MOUSEMOTION:
            *window_id = ev->motion.windowID;
            *x = ev->motion.x;
            *y = ev->motion.y;
            *delta_x = ev->motion.xrel;
            *delta_y = ev->motion.yrel;
            return 1;
        case SDL_MOUSEBUTTONDOWN:
        case SDL_MOUSEBUTTONUP:
            *class = (ev->type == SDL_MOUSEBUTTONDOWN)?
                MSDL_MOUSEEV_DOWN : MSDL_MOUSEEV_UP;
            *window_id = ev->button.windowID;
            *id = ev->button.button;
            *x = ev->button.x;
            *y = ev->button.y;
            return 1;
        default:
            return 0;
    }
}

#define MSDL_EV_QUIT 1
#define MSDL_EV_POINT 2
#define MSDL_EV_KEY_CHAR 3
#define MSDL_EV_KEY_EVENT 4
MOSHEXPORT
int /* 0 for unknown event */
msdl_event_read_type(void* p){
    SDL_Event* ev = (SDL_Event *)p;
    switch(ev->type){
        case SDL_KEYDOWN:
        case SDL_KEYUP:
            switch(ev->key.keysym.sym){
                case SDLK_END:
                case SDLK_ESCAPE:
                case SDLK_HOME:
                case SDLK_DELETE:
                case SDLK_BACKSPACE:
                case SDLK_LALT:
                case SDLK_LCTRL:
                case SDLK_LSHIFT:
                case SDLK_PAGEUP:
                case SDLK_PAGEDOWN:
                case SDLK_RALT: /* Option key */
                case SDLK_RCTRL:
                case SDLK_RSHIFT:
                case SDLK_RETURN: /* Enter key */

                /* direction keys */
                case SDLK_LEFT:
                case SDLK_RIGHT:
                case SDLK_UP:
                case SDLK_DOWN:
                    return MSDL_EV_KEY_EVENT;
                default:
                    return MSDL_EV_KEY_CHAR;
            }
        case SDL_MOUSEMOTION:
        case SDL_MOUSEBUTTONDOWN:
        case SDL_MOUSEBUTTONUP:
        case SDL_MOUSEWHEEL:
            return MSDL_EV_POINT;
        case SDL_QUIT:
            return MSDL_EV_QUIT;
        default:
            return 0;
    }
}

MOSHEXPORT
int /* 1 for event */
msdl_event_poll(void* ev){
    return SDL_PollEvent((SDL_Event *) ev);
}

MOSHEXPORT
int 
msdl_event_size(void){
    return(sizeof(SDL_Event));
}

MOSHEXPORT
void* /* NB: Don't have to dispose! */
msdl_window_surface_get(void* win){
    return SDL_GetWindowSurface((SDL_Window *)win);
}

MOSHEXPORT
int
msdl_window_id(void* win){
    return SDL_GetWindowID((SDL_Window *)win);
}

MOSHEXPORT
void
msdl_window_update(void* win, void* from, void* to,
                   int x, int y, int w, int h,
                   int x1, int y1){
    SDL_Rect src;
    SDL_Rect dest;
    src.x = x;
    src.y = y;
    src.w = w;
    src.h = h;
    dest.x = x1;
    dest.y = y1;
    SDL_BlitSurface((SDL_Surface *)from,&src,(SDL_Surface *)to,&dest);
    SDL_UpdateWindowSurfaceRects((SDL_Window *)win,&dest,1);
}

MOSHEXPORT
void
msdl_window_destroy(void* win){
    SDL_DestroyWindow((SDL_Window*)win);
}

MOSHEXPORT
void
msdl_window_show(void* win){
    SDL_ShowWindow((SDL_Window *)win);
}

MOSHEXPORT
void*
msdl_window_create(void* title,int x, int y, int w, int h){
    SDL_Window* win;
    if(x<0){
        win = SDL_CreateWindow(title,SDL_WINDOWPOS_UNDEFINED,
                               SDL_WINDOWPOS_UNDEFINED,
                               w,h, SDL_WINDOW_RESIZABLE);
    }else{
        win = SDL_CreateWindow(title,x,y,w,h, SDL_WINDOW_RESIZABLE);
    }
    return win;
}

MOSHEXPORT
int
msdl_init(void){
    return SDL_Init(SDL_INIT_EVERYTHING);
}

#if 0 /* Async version (i think it won't work in OSX) */
int initialized = 0;

#define MSDL_EV_QUIT 1
#define MSDL_EV_CLOSE 2
#define MSDL_EV_CREATE 3
#define MSDL_EV_UPDATE 4

#define MSDL_2 /* SDL2 version */

typedef struct __msdl_window_param{
    int autopos_p;
    int x;
    int y;
    int w;
    int h;
    char title[1];
}msdl_window_param;

typedef struct __msdl_update_param{
    void* from;
    void* to;
    void* win;
    int x;
    int y;
    int w;
    int h;
    int x1;
    int y1;
}msdl_update_param;

static void
push_event(int code, void* p){
    SDL_Event ev;
    ev.type = SDL_USEREVENT;
    ev.user.code = code;
    ev.user.data1 = p;
    SDL_PushEvent(&ev);
}

MOSHEXPORT
void
msdl_enqueue_update(void* win, void* from, void* to, int x, int y,int w,int h,int x1, int y1){
    msdl_update_param* p;
    p = malloc(sizeof(msdl_update_param));

    p->from = from;
    p->win = win;
    p->to = to;
    p->x = x;
    p->y = y;
    p->w = w;
    p->h = h;
    p->x1 = x1;
    p->y1 = y1;
    push_event(MSDL_EV_UPDATE,p);
}

MOSHEXPORT
void
msdl_enqueue_window_create(char* title,int x,int y,int w,int h,int flags){
    msdl_window_param* p;
    (void)flags;
    p = malloc(sizeof(msdl_window_param)+strlen(title));
    if(x<0){
        p->autopos_p = 1;
    }
    p->x = x;
    p->y = y;
    p->w = w;
    p->h = h;
    strcpy(p->title,title);
    push_event(MSDL_EV_CREATE, p);
}



static int
sdl_event_passthrough(void* bogus, SDL_Event* evt){
    return 1;
}
static uintptr_t
coreloop(uintptr_t in0, uintptr_t in1, uintptr_t* out0, uintptr_t * out1){
    /* out0 == 0 means SDL exception.
     * out1:
     *    0: SDL Error
     *    1: SDL Initialize done.
     *    2: SDL Surface enqueue done.
     *
     * out0 == 1 means SDL window create callback.
     * out1 == pointer to SDL_Window
     */
    SDL_Event* ev;
    SDL_Window* win;
    SDL_Surface* from;
    SDL_Surface* to;
    SDL_Rect src;
    SDL_Rect dest;
    msdl_window_param* param;
    msdl_update_param* update;
    int x;
    int y;
    int w;
    int h;
    int x1;
    int y1;

    int r;
    if(!initialized){
        r = SDL_Init(SDL_INIT_VIDEO);
        //SDL_SetEventFilter(&sdl_event_passthrough, NULL);
        initialized = 1;
        *out0 = 0;
        *out1 = 1;
    }else{
        ev = malloc(sizeof(SDL_Event));
        r = SDL_WaitEvent(ev);
        if(r == 1){
            printf("event: %d\n",ev->type);
            if(ev->type == SDL_USEREVENT){
                *out0 = 0;
                switch(ev->user.code){
                    case MSDL_EV_CREATE:
                        param = (msdl_window_param *)ev->user.data1;
                        if(param->autopos_p){
                            x = SDL_WINDOWPOS_UNDEFINED;
                            y = SDL_WINDOWPOS_UNDEFINED;
                        }else{
                            x = param->x;
                            y = param->y;
                        }
                        w = param->w;
                        h = param->h;
                        /* FIXME: Fill flags */
                        win = SDL_CreateWindow(param->title,x,y,w,h,
                                               SDL_WINDOW_RESIZABLE
                                               );
                        free(param);
                        *out0 = 1;
                        *out1 = (uintptr_t)win;
                        break;
                    case MSDL_EV_UPDATE:
                        update = (msdl_update_param *)ev->user.data1;
                        from = update->from;
                        to = update->to;
                        src.x = update->x;
                        src.y = update->y;
                        src.w = update->w;
                        src.h = update->h;
                        dest.x = update->x1;
                        dest.y = update->y1;
                        SDL_BlitSurface(from,&src,to,&dest);
                        win = update->win;
                        SDL_UpdateWindowSurfaceRects(win,&dest,1);
                        free(update);
                        break;
                }
                free(ev);
            }else{
                *out0 = (uintptr_t)ev;
                *out1 = SDL_GetTicks();
            }
        }else{
            free(ev);
            *out0 = 0;
            *out1 = 0;
        }
    }
    return 1;
}

MOSHEXPORT
void
msdl_event_dispose(void* p){
    free(p);
}

MOSHEXPORT
void*
msdl_getcoreloop_func(void){
    return &coreloop;
}
#endif
