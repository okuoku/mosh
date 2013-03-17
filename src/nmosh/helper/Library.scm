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
  (void moshvm_start_detached (void* int void* void* void*))
  (void* moshvm_join (void*))
  (void moshvm_sharedstorage_init (void*))
  (void moshvm_sharedstorage_get (void* void*))
  (void* moshvm_stacktrace_get (void*))
  (void* moshvm_getmainvm)
  ;; Pseudo: Will not be called from any Scheme code
  (void* moshvm_export_object (void*))
  (void* moshvm_callback_call (void* void*))
  (void* moshvm_execute_callback (void* void*))
  ;; interrupt
  (void moshvm_keyboard_interrupt_enable)
  (void moshvm_interrupt (void*))
  )
