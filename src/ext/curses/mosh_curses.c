#include "config.h"
#include <stdint.h>
#include <wchar.h>

/* PDCurses settings */
#define PDC_WIDE 1
#define NCURSES_MOUSE_VERSION 1

#if defined(HAVE_NCURSESW_CURSES_H)
#  include <ncursesw/curses.h>
#elif defined(HAVE_NCURSESW_H)
#  include <ncursesw.h>
#elif defined(HAVE_NCURSES_CURSES_H)
#  include <ncurses/curses.h>
#elif defined(HAVE_NCURSES_H)
#  include <ncurses.h>
#elif defined(HAVE_CURSES_H)
#  include <curses.h>
#else
#include <curses.h>
#endif

#if 0
MOSHEXPORT
void*
mcur_initscr(void){
#ifdef XCURSES /* for PDCurses ...*/
    return Xinitscr();
#else
    return initscr();
#endif

}
#endif /* don't use */

#if 0
MOSHEXPORT
void
mcur_endwin(void){
    endwin();
}
#endif /* don't use */

MOSHEXPORT
int
mcur_getch(void){
    return getch();
}

void 
mcur_acquire(void){
#ifdef XCURSES /* for PDCurses ...*/
    Xinitscr();
#else
    initscr();
#endif
    raw();
    noecho();
    cbreak();
    nonl();
    keypad(stdscr, TRUE);
    /* nodelay(stdscr, TRUE); */
    if(has_colors()){ start_color(); }
}

MOSHEXPORT
void
mcur_release(void){
    endwin();
}

MOSHEXPORT
void
mcur_refresh(void){
    refresh();
}


MOSHEXPORT
void
mcur_mouse_enable(void){
    mousemask(ALL_MOUSE_EVENTS, NULL);
}

MOSHEXPORT
void
mcur_mouse_disable(void){
    mousemask(0, NULL);
}

MOSHEXPORT
int
mcur_lines(void){
    return LINES;
}

MOSHEXPORT
int
mcur_cols(void){
    return COLS;
}

MOSHEXPORT
void
mcur_locate(int x,int y){
    move(y, x);
}

MOSHEXPORT
void
mcur_cls(void){
    erase();
}

MOSHEXPORT
int
mcur_colors(void){
    return COLORS;
}

MOSHEXPORT
int
mcur_color_pairs(void){
    return COLOR_PAIRS;
}

MOSHEXPORT
int
mcur_color_configurable_p(void){
    return can_change_color();
}

MOSHEXPORT
void
mcur_color_content(int c,int *r,int *g,int *b){
    short rr;
    short gg;
    short bb;
    color_content(c,&rr,&gg,&bb);
    *r = rr;
    *g = gg;
    *b = bb;
}

MOSHEXPORT
void
mcur_pair_content(int c, int* fg, int *bg){
    short ffg;
    short bbg;
    pair_content(c, &ffg, &bbg);
    *fg = ffg;
    *bg = bbg;
}

MOSHEXPORT
void
mcur_init_pair(int c, int fg, int bg){
    init_pair(c,fg,bg);
}

MOSHEXPORT
void
mcur_init_color(int c,int r,int g,int b){
    init_color(c,r,g,b);
}

MOSHEXPORT
int
mcur_attrib_normal(void){
    return A_NORMAL;
}

MOSHEXPORT
int
mcur_attrib_standout(void){
    return A_STANDOUT;
}

MOSHEXPORT
int
mcur_attrib_underline(void){
    return A_UNDERLINE;
}

MOSHEXPORT
int
mcur_attrib_reverse(void){
    return A_REVERSE;
}

MOSHEXPORT
int
mcur_attrib_blink(void){
    return A_BLINK;
}

MOSHEXPORT
int
mcur_attrib_dim(void){
    return A_DIM;
}

MOSHEXPORT
int
mcur_attrib_bold(void){
    return A_BOLD;
}

MOSHEXPORT
int
mcur_attrib_protect(void){
    return A_PROTECT;
}

MOSHEXPORT
int
mcur_attrib_invis(void){
    return A_INVIS;
}

MOSHEXPORT
int
mcur_attrib_altcharset(void){
    return A_ALTCHARSET;
}

MOSHEXPORT
int
mcur_attrib_color_pair(int c){
    return COLOR_PAIR(c);
}

MOSHEXPORT
void
mcur_attron(int a){
    attron(a);
}

MOSHEXPORT
void
mcur_attroff(int a){
    attroff(a);
}

MOSHEXPORT
void
mcur_attrset(int a){
    attrset(a);
}

MOSHEXPORT
void
mcur_print(char *s){
    printw(s);
}

MOSHEXPORT
int
mcur_pdcurses(void){
#ifdef PDCURSES
    return 1;
#else
    return 0;
#endif
}

MOSHEXPORT
void
mcur_set_title(char *s){
#ifdef PDCURSES
    PDC_set_title(s);
#endif
}

MOSHEXPORT
int
mcur_key_mouse(void){
    return KEY_MOUSE;
}

MOSHEXPORT
int
mcur_key_resize(void){
    return KEY_RESIZE;
}


MOSHEXPORT
void
mcur_resize_term(void){
#ifdef PDCURSES
    resize_term(0,0);
#endif
}

static int acquired = 0;
static uintptr_t
keyloop(uintptr_t in0,uintptr_t in1,uintptr_t* out0,uintptr_t* out1){
    int r;
    wint_t w;
    if(!acquired){
        acquired = 1;
        mcur_acquire();
        *out0 = 0;
        *out1 = 0;
        return 1;
    }
    // FIXME: Use get_wch ??
    w = getch();
    *out0 = 0;
    *out1 = w;
    return 1;
}

MOSHEXPORT
void*
mcur_getkeyloop(void){
    return &keyloop;
}
