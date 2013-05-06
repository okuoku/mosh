(mosh-libcurl
  c-function-table
  *internal*
  (plugin: mosh_libcurl)
  (libname: mosh_libcurl)
  (c-import: 
    "mcurl.c"
    "mcurl_multi.c"
    "mcurl_easy.c"
    "mcurl_global.c"
    "mcurl_form.c")
  #(ret name args)
  (void* mcurl_easy_init)
  (void mcurl_easy_cleanup (void*))
  (int mcurl_easy_perform (void*))
  (int mcurl_easy_setopt (void* int void*))

  (void mcurl_form_free (void*))
  (int mcurl_formadd_cstring (void* void* void* void*))
  (int mcurl_formadd_blob (void* void*void* void* int void* void*))

  (int mcurl_global_init (int))
  (void mcurl_global_cleanup)
  )
