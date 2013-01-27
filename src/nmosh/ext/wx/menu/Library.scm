(mosh-wx-icon
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx menu)
  (parent-libname: mosh_wx)
  #(ret name args)
  (void* mwx_menuitem_create_submenu (int void* void* void*))
  (void* mwx_menuitem_create (int void* void* int))
  (void mwx_menu_item_append (void* void*))
  (void mwx_menu_item_append_separator (void*))
  (void mwx_menu_item_delete (void* int))
  (void* mwx_menu_create (void*))
  ;; Menubar
  (void* mwx_menubar_create)
  (void mwx_menubar_menu_append (void* void* void*))
  )
