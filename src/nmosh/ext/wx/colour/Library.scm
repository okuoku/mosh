(mosh-wx-colour
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx colours)
  (parent-libname: mosh_wx)
  ;(c-import: "mwx_colour.cpp")
  #(ret name args)
  (void* mwx_colour_null)
  (void* mwx_colour_create (int int int int))
  (void mwx_colour_destroy (void*))
  )
