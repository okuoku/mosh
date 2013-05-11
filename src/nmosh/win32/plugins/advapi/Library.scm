(mosh-winadvapi
  c-function-table
  *internal*
  (c-import: "mosh_advapi.c")
  (plugin: mosh_advapi)
  (libname: mosh_advapi)
  #(ret name args)
  (int madvapi_elevate)
  )
