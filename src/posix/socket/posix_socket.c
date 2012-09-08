#include "config.h"
#ifdef HAVE_SOCKET

#include "posix/socket/posix_socket.h"

#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <fcntl.h>
#include <gc.h>

/*** Socket Functions ... ***/

static struct addrinfo * 
save_addrinfo(struct addrinfo *ai, int do_free_p){
    struct addrinfo *p;
    if(!ai) return NULL;
    p = GC_MALLOC(sizeof(struct addrinfo));
    *p = *ai;
    if(ai->ai_addr){
        p->ai_addr = GC_MALLOC(ai->ai_addrlen);
        memcpy(p->ai_addr,ai->ai_addr,ai->ai_addrlen);
    }
    if(ai->ai_canonname){
        p->ai_canonname = GC_MALLOC(strlen(ai->ai_canonname));
        strcpy(p->ai_canonname,ai->ai_canonname);
    }
    if(ai->ai_next){
        p->ai_next = save_addrinfo(ai->ai_next,0);
    }
    if(do_free_p) freeaddrinfo(ai);
    return p;
}

int
socket_name_family(void *ap){
    struct addrinfo *ai = (struct addrinfo *)ap;
    switch(ai->ai_family){
        case AF_INET:
            return 4;
        case AF_INET6:
            return 6;
        default:
            return 0;
    }
}

/* proto: 1:TCP 2:UDP */
int
socket_getaddrinfo(char* name,char* servicename, void* ret_addrinfo, int mode, int proto){
    int ret;
    struct addrinfo ai;
    struct addrinfo* r = NULL;
    struct addrinfo* s;
    memset(&ai,0,sizeof(ai));
    switch(mode){
        case 0:
            ai.ai_family = PF_UNSPEC;
            break;
        case 4:
            ai.ai_family = PF_INET;
            break;
        case 6:
            ai.ai_family = PF_INET6;
            break;
    }
    switch(proto){
        case 0:
            ai.ai_family = 0;
            ai.ai_protocol = 0;
            break;
        case 1:
            ai.ai_socktype = SOCK_STREAM;
            ai.ai_protocol = IPPROTO_TCP;
            break;
        case 2:
            ai.ai_socktype = SOCK_DGRAM;
            ai.ai_protocol = IPPROTO_UDP;
            break;
    }

    ret = getaddrinfo(name,servicename,&ai,&r);
    s = save_addrinfo(r,1);
    *(void **)ret_addrinfo = s;
    return ret;
}

int
socket_create(int mode,int proto){
    int aaf;
    int aproto;
    int atype;
    int ret;
    switch(mode){
        case 0:
            aaf = PF_UNSPEC;
            break;
        case 4:
            aaf = PF_INET;
            break;
        case 6:
            aaf = PF_INET6;
            break;
        default:
            return -1;
    }
    switch(proto){
        case 1:
            atype = SOCK_STREAM;
            aproto = IPPROTO_TCP;
            break;
        case 2:
            atype = SOCK_DGRAM;
            aproto = IPPROTO_UDP;
            break;
        default:
            return -1;
    }
    /* FIXME: Do not set nonblock here.. (queue-listen/accept support needed.) */
    ret = socket(aaf,atype,aproto);

    return ret;
}

int
socket_getsockname(int s, void* buf, int bufsize){
    int size = bufsize;
    return getsockname(s, (struct sockaddr *)buf, &size);
}


void
socket_freeaddrinfo(void* ai){
    // DO NOTHING
    //freeaddrinfo(ai);
}

int
socket_bind(int fd,void* name,int len){
    int ret;
    ret = bind(fd,name,len);
    return ret; // =! 0, error
}

int
socket_listen(int fd,int l){
    return listen(fd,(l == 0)?SOMAXCONN:l);
}

int
socket_sizeof_sockaddr_storage(void){
    return sizeof(struct sockaddr_storage);
}

// 0: Connected, 1:DELAY, otherwise: error
int
socket_connect(int fd,void* name,int len){
    int ret;
    /* FIXME: move to creation phase */
    int i;
    i = fcntl(fd,F_GETFL);
    i |= O_NONBLOCK;
    fcntl(fd,F_SETFL,i);

    ret = connect(fd,name,len);
    if(ret == -1){
        if(errno == EINPROGRESS){
            return 1;
        }else{
            return -1;
        }
    }else{
        return 0;
    }
}

int // 0:DELAY, 0<:fd, otherwise: error
socket_accept(int fd,void* ret_name,int* ret_len){
    int ret;
    ret = accept(fd,ret_name,ret_len);
    if(ret < 0){
        if(errno == EWOULDBLOCK){
            return 0;
        }else{
            return ret;
        }
    }else{
        return ret;
    }
}

void
socket_sockaddr_read(void* sp,int *ret_family, void** ret_addr,int* ret_len,int* ret_port){
    struct sockaddr* sa = (struct sockaddr *)sp;
    struct sockaddr_in* s4 = (struct sockaddr_in *)sp;
    struct sockaddr_in6* s6 = (struct sockaddr_in6 *)sp;

    switch(sa->sa_family){
        case AF_INET:
            *ret_family = 4;
            memcpy(ret_addr,&s4->sin_addr,4);
            *ret_len = 4;
            *ret_port = s4->sin_port;
            break;
        case AF_INET6:
            *ret_family = 6;
            memcpy(ret_addr,&s6->sin6_addr,16);
            *ret_len = 16;
            *ret_port = s6->sin6_port;
            break;
        default: /* Domain socket ?? */
            *ret_family = 0;
            break;
    }
}

void
socket_addrinfo_read(void* aip,int *ret_family,void** ret_addr,int* ret_len,void** ret_next){
    struct addrinfo *ai;
    ai = (struct addrinfo *)aip;
    switch(ai->ai_family){
        case AF_INET:
            *ret_family = 4;
            break;
        case AF_INET6:
            *ret_family = 6;
            break;
        default:
            *ret_family = 0;
            break;
    }
    *(uintptr_t *)ret_addr = (uintptr_t)ai->ai_addr;
    *ret_len = ai->ai_addrlen;
    *(uintptr_t *)ret_next = (uintptr_t)ai->ai_next;
}

void
socket_setnodelay(int fd){
    int one = 1;
    setsockopt(fd,IPPROTO_TCP,TCP_NODELAY,&one,sizeof(int));
}

void
socket_setreuseaddr(int fd){
    int one = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR,&one,sizeof(int));
}

#endif
