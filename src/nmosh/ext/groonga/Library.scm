(mosh-groonga
  c-function-table
  *internal*
  (plugin: mosh_groonga)
  (libname: mosh-groonga)
  #(ret name args)
  (void mgrn_init)
  (void mgrn_result_free (void*))
  (void mgrn_request (void* void* int))
  (void* mgrn_create (void* void*))
  (void* mgrn_db_create_local (void* void*))
  (void* mgrn_db_open_local (void* void*))
  (void mgrn_dispose (void*))
  )
