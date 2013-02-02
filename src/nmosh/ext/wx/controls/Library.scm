(mosh-wx-control
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx controls)
  (parent-libname: mosh_wx)
  (c-import: "mwx_text.cpp")
  #(ret name args)
  (void* mwx_textctrl_create (void* void* int int))
  (void* mwx_textctrl_getvalue (void*))
  (void mwx_textctrl_setvalue (void* void*))
  (void mwx_textctrl_sethint (void* void*))
  )
