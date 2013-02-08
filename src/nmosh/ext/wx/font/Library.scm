(mosh-wx-notification
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx fonts)
  (parent-libname: mosh_wx)
  (c-import: "mwx_font.cpp")
  #(ret name args)
  (void* mwx_font_create (int int int int void* int))
  (void mwx_font_destroy (void*))
  (void* mwx_font_copy (void*))
  )
