#include <stdio.h>
#include <nmosh/vm-if.h>

int
main(int ac, char** av){
    int r;
    nmosh_object_t repl;
    nmosh_object_t param;
    nmosh_vmattr_t attr;
    nmosh_vm_t vm;
    NMOSH_EXPORT_BEGIN(px)
    NMOSH_EXPORT_END()
    nmosh_init();
    nmosh_vmattr_init(&attr);
    nmosh_vmattr_setverbose(attr, 1);
    nmosh_vmattr_setloadpath(attr, CHECKMOSH_LOADPATH);
    r = nmosh_vm_create(attr, &vm);
    printf("r = %d, vm = %lx\n",r,vm);
    nmosh_library_lookup(vm, "(nrepl simple)", "nrepl", &repl);
    printf("repl = %lx\n",repl);
    nmosh_object_export(px, &param);
    nmosh_vm_apply(vm, repl, param, NULL);
    return 0;
}
