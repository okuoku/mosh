(library (nmosh ext wx main)
         (export wx-main)
         (import (rnrs)
                 (nmosh stubs mosh_wx)
                 (nmosh ffi pffi-plugin)
                 (nmosh ffi pffi)
                 (nmosh pffi interface))
;; Wx GUI main

(define (wx-main cb)
  (define (starter . e)
    (cb) 
    ;; FIXME: Force continue
    #t)
  (mwx_startapp (make-callback starter) (object->pointer '())))

)
