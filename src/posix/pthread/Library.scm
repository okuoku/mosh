(posix-ffithread
  c-function-table
  *internal*
  (libname: posix-ffithread)
  (header: "posix/pthread/mosh_pthread.h")
  #(ret name args)
  (int posix_invoke_ffiqueue (int int int))
  (int posix_invoke_ffithread (int void* void* void*)))
  
