(mosh-wx-frame
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx frame)
  (parent-libname: mosh_wx)
  (c-import: "mwx_frame.cpp")
  #(ret name args)
  (void* mwx_frame_create (void* void* int int int int void* void* int))
  (void* mwx_frame_create_statusbar (void*))
  (void mwx_frame_set_menubar (void* void*))
  )
