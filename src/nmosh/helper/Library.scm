(moshvm-helper
  c-function-table
  *internal*
  (plugin: moshvm_helper)
  (libname: moshvm-helper)
  #(ret name args)
  (void* moshvm_alloc)
  (void moshvm_set_value_boolean (void* void* int))
  (void moshvm_set_value_string (void* void* void*))
  (void moshvm_start (void* int void*))
  (void* moshvm_join (void*))
  )
