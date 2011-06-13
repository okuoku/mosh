(kqueue-stubs
  c-function-table
  *internal*
  (libname: kqueue-stubs)
  (header: "kqueue_stubs.h")
  #(ret name args)
  (int kq_create)
  (void* kevent_alloc (int))
  (void* kevent_offset (void* int))
  (void kevent_dispose (void*))
  (void kevent_set_readevent (void* int))
  (void kevent_set_writeevent (void* int))
  (void kevent_set_enableuserevent (void* int))
  (void kevent_set_triggeruserevent (void* int))
  (int kevent_ident (void*))
  (int kevent_type (void*))
  (void kevent_decode_fd (void* void* void* void*))
  (int kevent_exec (int int void* int void* int))
  (int socket_sizeof_sockaddr_storage)
  (int socket_getaddrinfo (char* char* void* int int))
  (int socket_create (int int))
  (void socket_freeaddrinfo (void*))
  (int socket_bind (int void* int))
  (int socket_listen (int int))
  (int socket_connect (int void* int))
  (int socket_accept (int void* void*))
  (void socket_addrinfo_read (void* void* void* void* void*))
  (void socket_setnodelay (int))
  (int fd_read (int void* int))
  (int fd_write (int void* int))
  (int fd_close (int))
  (int fd_pipe (void* void*))
  (void fd_setnonblock (int)))
