(mosh-wx-window
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx window)
  (parent-libname: mosh_wx)
  (c-import: "mwx_window.cpp"
             "mwx_sizer.cpp"
             "../mwx_event.cpp")
  #(ret name args)
  (void* mwx_window_create_paintable (void* void* int))
  (void mwx_window_setfocus (void*))
  (void mwx_window_getid (void*))
  (void mwx_window_getsize (void* void* void*))
  (void mwx_window_refresh (void* int int int int))
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
  (int mwx_event_type (void*))
  (int mwx_event_mouse_wheel_delta (void*))
  (int mwx_event_mouse_wheel_axis (void*))
  (int mwx_event_mouse_x (void*))
  (int mwx_event_mouse_y (void*))
  )
