#include "nmosh/plugin-if.h"

#include "Object.h"
#include "Object-inl.h"
#include "Pair.h"
#include "Pair-inl.h"
#include "SString.h"
#include "ByteVector.h"
#include "VM.h"
#include "ByteVectorProcedures.h"
#include "ErrorProcedures.h"
#include "PortProcedures.h"
#include "ByteArrayBinaryInputPort.h"
#include "BinaryInputPort.h"
#include "Symbol.h"
#include "ProcedureMacro.h"
#include "Transcoder.h"
#include "UTF8Codec.h"
#include "UTF16Codec.h"
#include "UTF32Codec.h"
#include "Ratnum.h"
#include "Flonum.h"
#include "Bignum.h"
#include "Arithmetic.h"
#include "TextualOutputPort.h"
#include "FFI.h"
#include "Gloc.h"
#include "Closure.h"
#include "VM-inl.h"

#include "embed-libs.inc.h"

#if !defined(_WIN32) && !defined(MONA)
#define HAVE_TERMINAL // FIXME: this should be done in configure..
#endif

#ifndef MONA
#define HAVE_BDWGC_STUBS
#endif

#ifdef _WIN32
#define HAVE_AIO_WIN32
#include "win32/aio_win32.h"
#endif

#ifdef HAVE_TERMINAL
#include "posix/terminal/mosh_terminal.h"
#endif

#ifdef HAVE_BDWGC_STUBS
#include "generic/boehmgc-stubs/boehmgc-stubs.h"
#endif 

#ifdef HAVE_KQUEUE
#include "bsd/kqueue/kqueue_stubs.h"
#endif

#ifdef HAVE_PTRACE_COMMON
#include "posix/ptrace/ptrace_common.h"
#endif

#ifdef HAVE_POSIX_SPAWN
#include "posix/spawn/posixspawn.h"
#endif

#ifdef HAVE_FCNTL
#include "posix/fd/posix_fd.h"
#endif

#ifdef HAVE_POLL
#include "posix/poll/posix_poll.h"
#endif

#ifdef HAVE_SOCKET
#include "posix/socket/posix_socket.h"
#endif

#ifdef HAVE_SIGACTION
#include "posix/sigchld_handler/sigchld_handler.h"
#endif

#ifdef HAVE_VFORK
#include "posix/debugee/posix_debugee.h"
#include "posix/wait3/wait3.h" // FIXME: 
#include "posix/pthread/mosh_pthread.h" // FIXME:
#endif

using namespace scheme;

#define NIL Object::Nil
#define CONS(x,y) Object::cons((x),(y))
#define SYM(x) Symbol::intern(UC(x))
#define PTR(x) Object::makePointer((void*)x)
#define FUNC(x,y) CONS(SYM(x),PTR(y))
#define FN(x) FUNC(#x,x)

#define LIBDATA_PFFI_STUBS CONS(SYM("pffi-stubs"), \
CONS(FN(get_pffi_caller), \
    NIL))

#ifdef HAVE_TERMINAL
#define LIBDATA_TERMINAL CONS(SYM("terminal"), \
CONS(FN(terminal_serial_initialize), \
CONS(FN(terminal_serial_setspeed), \
CONS(FN(terminal_acquire), \
CONS(FN(terminal_release), \
CONS(FN(terminal_getsize), \
CONS(FN(terminal_getheight), \
CONS(FN(terminal_isatty), \
    NIL))))))))
#endif

#ifdef HAVE_PTRACE_COMMON 
#define LIBDATA_PTRACE_COMMON CONS(SYM("ptrace-common"), \
CONS(FN(call_ptrace), \
CONS(FN(ptrace_traceme), \
CONS(FN(ptrace_write), \
CONS(FN(ptrace_read), \
CONS(FN(ptrace_continue), \
CONS(FN(ptrace_singlestep), \
CONS(FN(ptrace_attach), \
CONS(FN(ptrace_detatch), \
CONS(FN(ptrace_regsize), \
CONS(FN(ptrace_getregs), \
CONS(FN(ptrace_setregs), \
CONS(FN(ptrace_fpregsize), \
CONS(FN(ptrace_getfpregs), \
CONS(FN(ptrace_setfpregs), NIL))))))))))))))) 
#endif

#ifdef HAVE_AIO_WIN32
#define LIBDATA_AIO_WIN32 CONS(SYM("aio-win32"), \
CONS(FN(win32_handle_open), \
CONS(FN(win32_handle_close), \
CONS(FN(win32_handle_setbaud), \
CONS(FN(win32_iocp_create), \
CONS(FN(win32_iocp_assoc), \
CONS(FN(win32_iocp_pop), \
CONS(FN(win32_overlapped_alloc), \
CONS(FN(win32_overlapped_free), \
CONS(FN(win32_overlapped_setmydata), \
CONS(FN(win32_overlapped_getmydata), \
CONS(FN(win32_overlapped_geterror), \
CONS(FN(win32_handle_read_async), \
CONS(FN(win32_handle_write_async), \
CONS(FN(win32_process_redirected_child2), \
CONS(FN(win32_create_named_pipe_async), \
CONS(FN(win32_wait_named_pipe_async), \
CONS(FN(win32_process_wait_async), \
CONS(FN(win32_sockaddr_storage_size), \
CONS(FN(win32_socket_create), \
CONS(FN(win32_socket_close), \
CONS(FN(win32_addrinfoex_free), \
CONS(FN(win32_addrinfoex_read), \
CONS(FN(win32_socket_connect), \
CONS(FN(win32_socket_accept), \
CONS(FN(win32_socket_bind), \
CONS(FN(win32_socket_listen), \
CONS(FN(win32_socket_getsockname), \
CONS(FN(win32_socket_setnodelay), \
CONS(FN(win32_socket_setreuseaddr), \
CONS(FN(win32_socket_recvfrom), \
CONS(FN(win32_socket_sendto), \
CONS(FN(win32_getaddrinfo), \
CONS(FN(win32_finalization_handler_get), \
CONS(FN(win32_finalization_handler_create), \
CONS(FN(win32_ticket_alloc), \
CONS(FN(win32_get_ticket_chime), \
CONS(FN(win32_timer_create), \
CONS(FN(win32_timer_set), \
CONS(FN(win32_timer_cancel), \
CONS(FN(win32_timer_destroy), \
	NIL)))))))))))))))))))))))))))))))))))))))))

#define LIBDATA_WIN32_GUI CONS(SYM("win32-gui"), \
CONS(FN(win32_messagebox) ,\
CONS(FN(win32_window_move) ,\
CONS(FN(win32_window_show) ,\
CONS(FN(win32_window_hide) ,\
CONS(FN(win32_window_settitle) ,\
CONS(FN(win32_window_close) ,\
CONS(FN(win32_window_destroy) ,\
CONS(FN(win32_registerwindowclass) ,\
CONS(FN(win32_window_alloc) ,\
CONS(FN(win32_window_create) ,\
CONS(FN(win32_window_fitbuffer) ,\
CONS(FN(win32_getmonitorinfo) ,\
CONS(FN(win32_window_updaterects) ,\
CONS(FN(win32_window_createbitmap) ,\
CONS(FN(win32_window_getwindowrect) ,\
CONS(FN(win32_window_getclientrect) ,\
CONS(FN(win32_window_getclientrect_x) ,\
CONS(FN(win32_window_getclientrect_y) ,\
CONS(FN(win32_window_clienttoscreen),\
CONS(FN(win32_dc_create) ,\
CONS(FN(win32_dc_dispose) ,\
CONS(FN(win32_dc_selectobject) ,\
CONS(FN(win32_dc_transform) ,\
CONS(FN(win32_dc_settransform) ,\
CONS(FN(win32_gdi_deleteobject) ,\
CONS(FN(win32_pen_create) ,\
CONS(FN(win32_brush_create) ,\
CONS(FN(win32_font_create) ,\
CONS(FN(win32_dc_draw) ,\
CONS(FN(win32_dc_measure_text) ,\
CONS(FN(win32_cursor_hide) ,\
	NIL))))))))))))))))))))))))))))))))

#define LIBDATA_WIN32_MISC CONS(SYM("win32-misc"), \
CONS(FN(win32_invoke_ffithread), \
CONS(FN(win32_get_processor_count), \
CONS(FN(win32_get_ansi_codepage), \
CONS(FN(win32_multibyte_to_widechar), \
CONS(FN(win32_measure_multibyte_to_widechar), \
CONS(FN(win32_mypath), \
CONS(FN(win32_setenv), \
CONS(FN(win32_querydosdevice), \
CONS(FN(win32_extent_size), \
CONS(FN(win32_extent_get), \
CONS(FN(win32_extent_disknumber), \
CONS(FN(win32_extent_offset), \
CONS(FN(win32_extent_length), \
CONS(FN(win32_console_getpalette), \
CONS(FN(win32_console_setpalette), \
CONS(FN(win32_console_setcolor), \
CONS(FN(win32_console_p), \
CONS(FN(win32_console_getsize), \
CONS(FN(win32_console_setpos), \
CONS(FN(win32_console_settitle), \
CONS(FN(win32_getstdhandle), \
CONS(FN(win32_console_release), \
CONS(FN(win32_console_acquire), \
CONS(FN(win32_get_console_reader_func), \
CONS(FN(win32_console_output), \
CONS(FN(win32_console_vscroll), \
CONS(FN(win32_clipboard_text_set), \
CONS(FN(win32_dl_open), \
CONS(FN(win32_dl_lookup), \
	NIL))))))))))))))))))))))))))))))
#endif

#define LIBDATA_BOEHMGC_STUBS CONS(SYM("boehmgc-stubs"), \
CONS(FN(create_weak_vector), \
CONS(FN(weak_vector_ref), \
CONS(FN(weak_vector_set), \
CONS(FN(register_disappearing_link_wv), \
CONS(FN(register_finalizer), \
CONS(FN(register_disappearing_link), \
CONS(FN(gcollect), \
CONS(FN(genable_incremental), \
CONS(FN(gfree_size), \
CONS(FN(gset_time_limit), \
CONS(FN(gcurrent_size), NIL))))))))))))

#ifdef HAVE_KQUEUE
#define LIBDATA_KQUEUE CONS(SYM("kqueue-stubs"), \
CONS(FN(kq_create), \
CONS(FN(kevent_alloc), \
CONS(FN(kevent_offset), \
CONS(FN(kevent_dispose), \
CONS(FN(kevent_set_readevent), \
CONS(FN(kevent_set_writeevent), \
CONS(FN(kevent_set_enableuserevent), \
CONS(FN(kevent_set_triggeruserevent), \
CONS(FN(kevent_ident), \
CONS(FN(kevent_type), \
CONS(FN(kevent_decode_fd), \
CONS(FN(kevent_exec), \
    NIL)))))))))))))
#endif

#ifdef HAVE_SOCKET
#define LIBDATA_POSIX_SOCKET CONS(SYM("posix-socket"), \
CONS(FN(socket_sizeof_sockaddr_storage), \
CONS(FN(socket_getaddrinfo), \
CONS(FN(socket_create), \
CONS(FN(socket_freeaddrinfo), \
CONS(FN(socket_bind), \
CONS(FN(socket_accept), \
CONS(FN(socket_listen), \
CONS(FN(socket_connect), \
CONS(FN(socket_addrinfo_read), \
CONS(FN(socket_setnodelay), \
CONS(FN(socket_setreuseaddr), \
CONS(FN(socket_sockaddr_read), \
CONS(FN(socket_name_family), \
CONS(FN(socket_getsockname), \
    NIL)))))))))))))))
#endif

#ifdef HAVE_POLL
#define LIBDATA_POSIX_POLL CONS(SYM("posix-poll"), \
CONS(FN(poll_alloc), \
CONS(FN(poll_dispose), \
CONS(FN(poll_exec), \
CONS(FN(poll_set_fd), \
CONS(FN(poll_set_pollin), \
CONS(FN(poll_unset_pollin), \
CONS(FN(poll_set_pollout), \
CONS(FN(poll_unset_pollout), \
CONS(FN(poll_get_pollin), \
CONS(FN(poll_get_pollout), \
CONS(FN(poll_get_pollerr), \
CONS(FN(poll_get_pollhup), \
CONS(FN(poll_get_pollnval), \
    NIL))))))))))))))
#endif

#ifdef HAVE_FCNTL
#define LIBDATA_POSIX_FD CONS(SYM("posix-fd"), \
CONS(FN(fd_open), \
CONS(FN(fd_open_rw), \
CONS(FN(fd_close), \
CONS(FN(fd_read), \
CONS(FN(fd_write), \
CONS(FN(fd_close), \
CONS(FN(fd_setnonblock), \
CONS(FN(fd_pipe), \
    NIL)))))))))
#endif

#ifdef HAVE_POSIX_SPAWN
#define LIBDATA_POSIX_SPAWN CONS(SYM("posixspawn"), \
CONS(FN(posixspawn_spawn), \
CONS(FN(posixspawn_fileactionssize), \
CONS(FN(posixspawn_fileactions_init), \
CONS(FN(posixspawn_fileactions_destroy), \
CONS(FN(posixspawn_fileactions_adddup2), \
CONS(FN(posixspawn_fileactions_addclose), \
    NIL)))))))
#endif

#ifdef HAVE_SIGACTION
#define LIBDATA_POSIX_SIGCHLD_HANDLER CONS(SYM("posix-sigchld-handler"), \
CONS(FN(sigchld_handler_install),NIL))
#endif

#ifdef HAVE_VFORK
#define LIBDATA_POSIX_DEBUGEE CONS(SYM("posix-debugee"), \
CONS(FN(debugee_spawn), \
CONS(FN(debugee_fileactionssize), \
CONS(FN(debugee_fileactions_init), \
CONS(FN(debugee_fileactions_destroy), \
CONS(FN(debugee_fileactions_adddup2), \
CONS(FN(debugee_fileactions_addclose), \
    NIL)))))))
#define LIBDATA_POSIX_WAIT3 CONS(SYM("wait3"), \
CONS(FN(size_rusage), \
CONS(FN(try_wait3), \
     NIL)))
#define LIBDATA_POSIX_FFITHREAD CONS(SYM("posix-ffithread"), \
CONS(FN(posix_invoke_ffithread), \
CONS(FN(posix_invoke_ffiqueue), \
     NIL)))
#endif

#include "call-stubs.inc.c" /* Generated by misc/scripts/gen-nmosh-stubs.sps */

typedef void (*pffi_caller_t)(void* func, void* args, void* ret);
typedef struct {
    void* gate;
    void* func;
    void* args;
    void* ret;
} pffi_caller_data;
static void
pffi_caller(uintptr_t in0, uintptr_t in1, uintptr_t* out0, uintptr_t* out1){
    // NB: This function will be called from ffi queue thread
    pffi_caller_t call;
    pffi_caller_data* d = (pffi_caller_data *)in0;
    call = (pffi_caller_t)d->gate;
    call(d->func,d->args,d->ret);
}

static void*
get_pffi_caller(void){
    return (void*)&pffi_caller;
}

static Object
stub_pffi_call(VM* theVM, int argc, const Object* argv){
    pffi_caller_t call;
    DeclareProcedureName("%nmosh-pffi-call");
    checkArgumentLength(4);
    argumentAsPointer(0, gate);
    argumentAsPointer(1, func);
    argumentAsPointer(2, args);
    argumentAsPointer(3, ret);
    call = (pffi_caller_t)(gate->pointer());
    call((void*)func->pointer(), (void*)args->pointer(), (void*)ret->pointer());
    return Object::True;
}

static Object
stub_get_pffi_feature_set(VM* theVM, int argc, const Object* argv){
    //DeclareProcedureName("%get-pffi-feature-set");
    Object tmp;

    tmp = Object::Nil;
    tmp = Object::cons(LIBDATA_PFFI_STUBS,tmp);
#ifdef HAVE_PTRACE_COMMON
    tmp = Object::cons(LIBDATA_PTRACE_COMMON,tmp);
#endif
#ifdef HAVE_KQUEUE
    tmp = Object::cons(LIBDATA_KQUEUE,tmp);
#endif
#ifdef HAVE_FCNTL
    tmp = Object::cons(LIBDATA_POSIX_FD,tmp);
#endif
#ifdef HAVE_BDWGC_STUBS
    tmp = Object::cons(LIBDATA_BOEHMGC_STUBS,tmp);
#endif
#ifdef HAVE_TERMINAL
    tmp = Object::cons(LIBDATA_TERMINAL,tmp);
#endif
#ifdef HAVE_AIO_WIN32
    tmp = Object::cons(LIBDATA_AIO_WIN32,tmp);
    tmp = Object::cons(LIBDATA_WIN32_GUI,tmp);
    tmp = Object::cons(LIBDATA_WIN32_MISC,tmp);
#endif
#ifdef HAVE_POSIX_SPAWN
    tmp = Object::cons(LIBDATA_POSIX_SPAWN,tmp);
#endif
#ifdef HAVE_POLL
    tmp = Object::cons(LIBDATA_POSIX_POLL,tmp);
#endif
#ifdef HAVE_SOCKET
    tmp = Object::cons(LIBDATA_POSIX_SOCKET,tmp);
#endif
#ifdef HAVE_SIGACTION
    tmp = Object::cons(LIBDATA_POSIX_SIGCHLD_HANDLER,tmp);
#endif
#ifdef HAVE_VFORK
    tmp = Object::cons(LIBDATA_POSIX_DEBUGEE,tmp);
    tmp = Object::cons(LIBDATA_POSIX_WAIT3,tmp);
    tmp = Object::cons(LIBDATA_POSIX_FFITHREAD,tmp);
#endif
#ifdef LIBDATA_CALL_STUBS
    tmp = Object::cons(LIBDATA_CALL_STUBS,tmp);
#endif
#include "embed-libs.inc.c"
    return tmp;
}

#undef NIL
#undef CONS
#undef SYM
#undef PTR

static Object
stub_pffi_callback_create(VM* theVM, int argc, const Object* argv){
    DeclareProcedureName("%nmosh-create-callback");
    argumentAsClosure(0, clo);
    return Object::cons(Object::makeVM(theVM), 
                        Object::makeClosure(clo));
}

void* nmosh_archive_ptr = NULL;
uintptr_t nmosh_archive_size = 0;

void
register_stubs(VM* theVM){
    // Standard pffi interface 
    theVM->setValueString(UC("%get-pffi-feature-set"),
                          Object::makeCProcedure(stub_get_pffi_feature_set));
    theVM->setValueString(UC("%nmosh-pffi-call"),
                          Object::makeCProcedure(stub_pffi_call));
    theVM->setValueString(UC("%nmosh-pffi-callback-create"),
                          Object::makeCProcedure(stub_pffi_callback_create));
    theVM->setValueString(UC("%nmosh-archive-pointer"),
                          Object::makePointer(nmosh_archive_ptr));
    theVM->setValueString(UC("%nmosh-archive-size"),
                          Object::makePointer(reinterpret_cast<void*>(nmosh_archive_size)));
}
