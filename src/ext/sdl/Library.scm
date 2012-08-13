(mosh-sdl
  c-function-table
  *internal*
  (plugin: mosh_sdl)
  (libname: mosh-sdl)
  #(ret name args)
  (int msdl_event_read_pointing (void* void* void* void* void* void* void*
                                       void* void*))
  (int msdl_event_read_type (void*))
  (int msdl_event_poll (void*))
  (int msdl_event_size)
  (void* msdl_window_surface_get (void*))
  (int msdl_window_id (void*))
  (void msdl_window_update (void* void* void* int int int int int int))
  (void msdl_window_destroy (void*))
  (void msdl_window_show (void*))
  (void* msdl_window_create (void* int int int int))
  (void* msdl_init)
  ;(void msdl_enqueue_update (void* void* void* int int int int int int))
  ;(void msdl_enqueue_window_create (void* int int int int int))
  ;(void msdl_window_show (void*))
  ;(void msdl_event_dispose (void*))
  ;(void* msdl_getcoreloop_func)
  )
