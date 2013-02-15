(mosh-wx-graphics
  c-function-table
  *internal*
  (plugin: mosh_wx)
  (libname: mosh_wx graphics)
  (parent-libname: mosh_wx)
  ;(c-import: "mwx_font.cpp")
  #(ret name args)
  (int mwx_graphics_kick (void* void* int void* int void* int))
  (void mwx_bitmap_destroy (void*))
  (void* mwx_bitmap_create (int int int))
  (void* mwx_bitmap_from_image (void*))
  (void mwx_image_init_handlers)
  (void mwx_image_destroy (void*))
  (void mwx_brush_destroy (void*))
  (void* mwx_brush_create (int int int int int))
  (void* mwx_pen_create (int int int int int int))
  (void mwx_pen_destroy (void*))
  )
