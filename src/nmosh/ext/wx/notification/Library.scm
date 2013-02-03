(mosh-wx-notification
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx notification)
  (parent-libname: mosh_wx)
  ;(c-import: "mwx_notification.cpp")
  #(ret name args)
  (void mwx_notification_show (void* int))
  (void mwx_notification_destroy (void*))
  (void* mwx_notification_create (void* void* int))
  (void mwx_notification_close (void*))
  )
