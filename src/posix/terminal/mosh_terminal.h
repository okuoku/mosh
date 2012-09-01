#ifndef _MOSH_TERMINAL_H
extern "C"{
void terminal_serial_initialize(int);
int terminal_serial_setspeed(int,int);
void terminal_acquire(void);
void terminal_release(void);
int terminal_getsize(void);
int terminal_getheight(void);
int terminal_isatty(int);
};
#endif
