#include "config.h"

#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <fcntl.h>

typedef uintptr_t (*ffithread_callback_t)(uintptr_t in0,uintptr_t in1,
                                          uintptr_t* out0,uintptr_t* out1);

typedef struct{
    uintptr_t in0;
    uintptr_t in1;
    uintptr_t func;
    uintptr_t fd;
} ffithread_data;

typedef struct{
    uintptr_t func;
    uintptr_t in0;
    uintptr_t in1;
} ffiqueue_command;

typedef struct{
    uintptr_t fd_cmd;
    uintptr_t fd_res;
} ffiqueue_data;

static void*
ffithread(void* arg){
    ffithread_data* d = (ffithread_data *)arg;
    int fd = d->fd;
    uintptr_t in0 = d->in0;
    uintptr_t in1 = d->in1;
    ffithread_callback_t callback = (ffithread_callback_t)d->func;
    uintptr_t out0;
    uintptr_t out1;
    uintptr_t r;
    char buf[sizeof(uintptr_t)*2];
    free(d);

    for(;;){
        r = callback(in0,in1,&out0,&out1);
        memcpy(buf,&out0,sizeof(uintptr_t));
        memcpy(&buf[sizeof(uintptr_t)],&out1,sizeof(uintptr_t));
        write(fd, buf, sizeof(buf));
        if(!r){
            return NULL;
        }
    }
}

static void*
ffiqueue(void* arg){
    int fd_cmd;
    int fd_res;
    int fl;
    int pos;
    int r;
    char* outbuf[sizeof(uintptr_t)*2];
    char buf[sizeof(ffiqueue_command)];
    ffiqueue_data* a = (ffiqueue_data *)arg;
    ffithread_callback_t callback;
    ffiqueue_command* d = (ffiqueue_command *)buf;
    fd_cmd = a->fd_cmd;
    fd_res = a->fd_res;
    free(arg);
    uintptr_t* out0 = (uintptr_t*)&outbuf[0];
    uintptr_t* out1 = (uintptr_t*)&outbuf[sizeof(uintptr_t)];
    /* set fd as blocking mode */
    fl = fcntl(fd_cmd, F_GETFL);
    fl &= (~O_NONBLOCK);
    fcntl(fd_cmd, F_SETFL, fl);
    for(;;){
        pos = 0;
        for(;;){
            r = read(fd_cmd, &buf[pos], sizeof(buf) - pos);
            if(r>=0){
                pos += r;
                if(pos == sizeof(ffiqueue_command)){
                    callback = (ffithread_callback_t)d->func;
                    if(!callback){
                        goto exit;
                    }
                    callback(d->in0,d->in1,out0,out1);
                    write(fd_res, outbuf,sizeof(outbuf));
                }else{
                    continue;
                }
            }else{
                goto exit;
            }
        }
    }
exit:
    close(fd_cmd);
    close(fd_res);
    return NULL;

}

int
posix_invoke_ffiqueue(int fd_cmd,int fd_res,int reserved){
    pthread_t bogus;
    pthread_attr_t attr;
    ffiqueue_data* d = malloc(sizeof(ffiqueue_data));

    d->fd_cmd = fd_cmd;
    d->fd_res = fd_res;

    pthread_attr_init(&attr);
    return pthread_create(&bogus, &attr, ffiqueue, (void*)d);
}

int
posix_invoke_ffithread(int fd,uintptr_t func,uintptr_t in0,uintptr_t in1){
    pthread_t bogus;
    pthread_attr_t attr;
    ffithread_data *d = malloc(sizeof(ffithread_data));

    d->fd = fd;
    d->in0 = in0;
    d->in1 = in1;
    d->func = func;

    pthread_attr_init(&attr);
    return pthread_create(&bogus, &attr, ffithread, d);
}

