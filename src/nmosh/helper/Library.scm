(moshvm-helper
  c-function-table
  *internal*
  ;; moshvm_helper cannot be a plugin for now.
  (plugin: moshvm_helper)
  (libname: moshvm-helper)
  #(ret name args)
  (void moshvm_longjmp (void*))
  (void* moshvm_alloc)
  (void moshvm_set_value_boolean (void* void* int))
  (void moshvm_set_value_string (void* void* void*))
  (void moshvm_set_value_pointer (void* void* void*))
  (void moshvm_start (void* int void*))
  (void* moshvm_join (void*))
  ;; Pseudo: Will not be called from any Scheme code
  (void* moshvm_export_object (void*))
  (void* moshvm_callback_call (void* void*))
  )
