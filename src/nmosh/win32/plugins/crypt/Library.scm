(mosh-wincrypt
  c-function-table
  *internal*
  (c-import: "mosh_wincrypt.c")
  (plugin: mosh_wincrypt)
  (libname: mosh_wincrypt)
  #(ret name args)
  (void madvapi_credfree (void*))
  (void madvapi_credblob (void* void* void*))
  (int madvapi_credread_generic (void* void*))
  (int madvapi_credwrite_generic (void* void* void* int int))
  )
