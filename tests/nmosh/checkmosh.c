#include <stdio.h>
#include <nmosh/vm-if.h>

void*
check(nmosh_object_cursor_t obj){
    int i;
    uint64_t u;
    nmosh_object_cursor_t cur;
    nmosh_object_cursor_t x;
    printf("Hi! ");
    cur = obj;
    while(nmosh_cursor_type(cur) == NMOSH_OBJECT_PAIR){
        nmosh_cursor_car(cur,&x);
        nmosh_cursor_cdr(cur,&cur);
        if(nmosh_cursor_type(x) == NMOSH_OBJECT_INTEGER){
            nmosh_cursor_integer_unsigned(x, &u);
            printf("%ld ",u);
        }
    }
    printf("\n");
    return NULL;
}

int
main(int ac, char** av){
    int r;
    nmosh_object_t run;
    nmosh_object_t param;
    nmosh_object_t outobj;
    nmosh_vmattr_t attr;
    nmosh_vm_t vm;
    NMOSH_EXPORT_BEGIN(out)
        NMOSH_EXPORT_POINTER(NULL, &check)
    NMOSH_EXPORT_END()
    NMOSH_EXPORT_BEGIN(px)
    NMOSH_EXPORT_END()
    nmosh_init();
    nmosh_vmattr_init(&attr);
    nmosh_vmattr_setverbose(attr, 1);
    nmosh_vmattr_setloadpath(attr, CHECKMOSH_LOADPATH);
    r = nmosh_vm_create(attr, &vm);
    printf("r = %d, vm = %lx\n",r,vm);
#ifdef CHECKMOSH_APPLET
    nmosh_library_lookup(vm, "(nmosh applet " CHECKMOSH_APPLET ")", 
                         CHECKMOSH_APPLET, &run);
    printf("applet = %lx\n",run);
#else
    nmosh_library_lookup(vm, "(nrepl simple)", "nrepl", &run);
    printf("repl = %lx\n",run);
#endif
    nmosh_object_export(out, &outobj);
    nmosh_vm_set(vm, "%check", outobj);
    nmosh_object_destroy(outobj);
    nmosh_object_export(px, &param); /* Nil */
    nmosh_vm_apply(vm, run, param, NULL);
    return 0;
}
