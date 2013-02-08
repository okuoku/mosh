(mosh-wx-control
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx controls)
  (parent-libname: mosh_wx)
  (c-import: "mwx_text.cpp"
             "mwx_textattr.cpp")
  #(ret name args)
  (void* mwx_textctrl_create (void* void* int int))
  (void* mwx_textctrl_getvalue (void*))
  (void mwx_textctrl_setvalue (void* void*))
  (void mwx_textctrl_sethint (void* void*))
  (void mwx_textctrl_appendtext (void* void*))
  (void mwx_textctrl_showposition (void* int))
  (int mwx_textctrl_getlastposition (void*))
  (void* mwx_textattr_create)
  (void mwx_textattr_destroy (void*))
  (void mwx_textattr_merge (void* void*))
  (void mwx_textattr_setfont (void* void*))
  )
