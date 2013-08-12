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
  (define null-list (object->pointer '()))
  (define obj-starter (make-callback starter))
  (mwx_startapp obj-starter null-list))

)
