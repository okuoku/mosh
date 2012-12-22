#ifdef __cplusplus
extern "C" {
#endif

int kq_create(void);
void* kevent_alloc(int n);
void* kevent_offset(void* ke,int n);
void kevent_dispose(void* p);
void kevent_set_readevent(void* ke,int fd);
void kevent_set_writeevent(void* ke,int fd);
void kevent_set_enableuserevent(void* ke,int id);
void kevent_set_triggeruserevent(void* ke,int id);
int kevent_ident(void* ke);
int kevent_type(void* ke);
void kevent_decode_fd(void* ke,int* type,int* eofp,int* data);
int kevent_exec(int q,int changecount, void* ke_changes,int count, void *ke_out,int timeout_ms);

#ifdef __cplusplus
}
#endif
