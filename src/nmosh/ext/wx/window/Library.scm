(mosh-wx-window
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx window)
  (parent-libname: mosh_wx)
  (c-import: "mwx_window.cpp")
  #(ret name args)
  (void mwx_window_show (void* int))
  )
