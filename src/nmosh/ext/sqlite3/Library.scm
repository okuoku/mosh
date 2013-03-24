(mosh-sqlite3
  c-function-table
  *internal*
  (plugin: mosh_sqlite3)
  (libname: mosh_sqlite3)
  (c-import: "msqlite3.c")
  #(ret name args)
  (void* msqlite3_strerr (int))
  (int msqlite3_open (void* void*))
  (int msqlite3_prepare_v2 (void* void* int void* void*))
  (int msqlite3_bind_blob (void* int void* int))
  (int msqlite3_bind_double (void* int double))
  (int msqlite3_bind_int (void* int int))
  (int msqlite3_bind_text (void* int void* int))
  (int msqlite3_bind_text16 (void* int void* int))
  (int msqlite3_bind_null (void* int))
  (void* msqlite3_column_blob (void* int))
  (int msqlite3_column_bytes (void* int))
  (int msqlite3_column_bytes16 (void* int))
  (double msqlite3_column_double (void* int))
  (int msqlite3_column_int (void* int))
  (void* msqlite3_column_text (void* int))
  (void* msqlite3_column_text16 (void* int))
  (int msqlite3_column_type (void* int))
  (int msqlite3_column_count (void*))
  (int msqlite3_step (void*))
  (int msqlite3_finalize (void*))
  (int msqlite3_close (void*))
  )
