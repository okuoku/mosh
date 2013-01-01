(mosh-sdl
  c-function-table
  *internal*
  (plugin: mosh_sdl_android)
  (libname: mosh-sdl-android)
  #(ret name args)
  (int msdl_android_asset_open (void* void*))
  (int msdl_android_asset_tell (void*))
  (int msdl_android_asset_seek (void* int int))
  (int msdl_android_asset_close (void*))
  (int msdl_android_asset_read (void* void* int))
  )
