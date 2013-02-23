(library (nmosh ffi c-callback)
         (export make-c-callback)
         (import (rnrs)
                 (nmosh pffi interface)
                 (nmosh stubs moshvm-helper))

(define (make-c-callback ptr)
  (lambda x
    (let ((r (moshvm_execute_callback ptr (object->pointer x))))
      (pointer->integer r)))) 
)
