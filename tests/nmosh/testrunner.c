#include <stdio.h>
#include <nmosh/vm-if.h>
#include "testrunner-loadpath.h"

int
main(int ac, char** av){
    int r;
    nmosh_object_t runner;
    nmosh_object_t param;
    nmosh_object_t outobj;
    nmosh_object_t cmdline;
    nmosh_object_t vmret;
    nmosh_vmattr_t attr;
    nmosh_vm_t vm;
    NMOSH_EXPORT_BEGIN(xcmdline)
        NMOSH_EXPORT_CSTRING(NULL, "invalid")
    NMOSH_EXPORT_END()
    NMOSH_EXPORT_BEGIN(px)
        NMOSH_EXPORT_CSTRING(NULL, TESTRUNNER_DIR)
        NMOSH_EXPORT_CSTRING(NULL, TESTRUNNER_ARG)
    NMOSH_EXPORT_END()
    nmosh_init();
    nmosh_vmattr_init(&attr);
    nmosh_vmattr_setverbose(attr, 0);
    nmosh_vmattr_setloadpath(attr, TESTRUNNER_LOADPATH);
    r = nmosh_vm_create(attr, &vm);
    nmosh_library_lookup(vm, "(nmosh internal testrunner)", "testrunner", 
                         &runner);
    nmosh_object_export(xcmdline, &cmdline);
    nmosh_vm_set(vm, "*command-line-args*", cmdline);
    nmosh_object_destroy(cmdline);
    nmosh_object_export(px, &param);
    nmosh_vm_apply(vm, runner, param, &vmret);
    nmosh_object_destroy(param);

    return 0;
}
