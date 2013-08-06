(mosh-winadvapi
  c-function-table
  *internal*
  (c-import: "mosh_advapi.c")
  (plugin: mosh_advapi)
  (libname: mosh_advapi)
  #(ret name args)
  (int madvapi_reg_open (void* void* int void*))
  (int madvapi_reg_close (void*))
  (int madvapi_reg_query (void* void* void* void* void*))
  )
