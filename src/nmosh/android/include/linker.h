#ifndef __NMOSH_LINKER_H_
#define __NMOSH_LINKER_H_

/* ELF bits */
#include <elf.h>
#include <sys/exec_elf.h>

struct link_map {
    /* Nmosh: stripped downed. just for Boehm GC */
    uintptr_t l_addr;
    struct link_map * l_next;
};

struct soinfo {
    /* Nmosh: soinfo from Bionic linker */
    /* FIXME: Is this really a abi?? */
    char name[128];
    void* phdr;
    int phnum;
    unsigned entry;
    unsigned base;
    unsigned size;

    /* Nmosh: We do not need after that */
};

typedef struct soinfo soinfo;

#endif /* __NMOSH_LINKER_H_ */
