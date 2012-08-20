#ifndef WIN32

#include <unistd.h>

#include <stdlib.h>
#include <stdio.h>

#include <termios.h>
#include <string.h>

#include <sys/ioctl.h> /* non posix */

/* from: http://www.adl.nii.ac.jp/~moro/unix-programmer/faq-j_4.html#SEC48 */

static struct termios stored_settings;

/* from: http://en.wikibooks.org/wiki/Serial_Programming/termios */
void
terminal_serial_initialize(int fd){
    struct termios settings;
    tcgetattr(fd, &settings);
    settings.c_iflag &= ~(IGNBRK | BRKINT | ICRNL |
            INLCR | PARMRK | INPCK | ISTRIP | IXON);
    settings.c_oflag = 0;
    settings.c_lflag  &= ~(ECHO | ECHONL | ICANON | IEXTEN | ISIG);
    settings.c_cflag &= ~(CSIZE | PARENB);
    settings.c_cflag |= CS8;
    settings.c_cc[VMIN] = 1;
    settings.c_cc[VTIME] = 0;
    tcsetattr(fd, TCSAFLUSH, &settings);
}

int
terminal_serial_setspeed(int fd, int speed){
    int mspeed;
    struct termios settings;
    switch(speed){
        /* FIXME: More baud rate! */
#ifdef B9600
        case 9600: 
            mspeed = B9600;
            break;
#endif
#ifdef B115200
        case 115200:
            mspeed = B115200;
            break;
#endif
        default:
            return 0;
    }
    tcgetattr(fd, &settings);
    cfsetispeed(&settings, mspeed);
    cfsetospeed(&settings, mspeed);
    tcsetattr(fd, TCSAFLUSH, &settings);
    return 1;
}

void 
terminal_acquire(void){
    struct termios new_settings;
    tcgetattr(0,&stored_settings);
    new_settings = stored_settings;
    new_settings.c_lflag &= (~ECHO); /* no echo */
    new_settings.c_lflag &= (~ICANON); /* no lined input */
    new_settings.c_lflag &= (~ISIG); /* no signal generation */
    new_settings.c_cc[VTIME] = 0;
    new_settings.c_cc[VMIN] = 1;
    tcsetattr(0,TCSANOW,&new_settings);
    return;
}

void 
terminal_release(void){
    tcsetattr(0,TCSANOW,&stored_settings);
    return;
}

int
terminal_getsize(void){
    struct winsize winsz;
    ioctl(1, TIOCGWINSZ, &winsz);
    return winsz.ws_col;
}

int
terminal_isatty(int fd){
    return isatty(fd);
}
#endif /* ifndef WIN32 */
