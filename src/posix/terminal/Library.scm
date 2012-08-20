(mosh-terminal
  c-function-table
  *internal*
  (libname: terminal)
  (header: "mosh_terminal.h")
  #(ret name args)
  (void terminal_serial_initialize (int))
  (int terminal_serial_setspeed (int int))
  (void terminal_acquire)
  (void terminal_release)
  (int  terminal_getsize)
  (int  terminal_isatty (int)))
  
