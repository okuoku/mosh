(mosh-wx-window
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx window)
  (parent-libname: mosh_wx)
  (c-import: "mwx_window.cpp"
             "mwx_sizer.cpp")
  #(ret name args)
  (void mwx_window_setsizer (void* void*))
  (void mwx_window_show (void* int))
  (void mwx_window_destroy (void*))
  (void* mwx_boxsizer_create (int))
  (void* mwx_staticboxsizer_create (void* int void*))
  (void* mwx_staticboxsizer_staticbox (void*))
  (void mwx_sizer_add_window (void* void* int int))
  (void mwx_sizer_add_sizer (void* void* int int))
  ;; in mwx_event
  (void mwx_event_skip (void*))
  )
