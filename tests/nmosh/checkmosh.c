#include <stdio.h>
#include <nmosh/vm-if.h>

int
main(int ac, char** av){
    int r;
    nmosh_vm_t vm;
    nmosh_init();
    r = nmosh_vm_create(NULL, &vm);
    printf("r = %d, vm = %lx\n",r,vm);
    return 0;
}
