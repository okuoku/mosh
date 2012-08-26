(import (rnrs)
        (mosh test)
        (nmosh pffi interface)
        (nmosh ffi box))

(define arch-64bit-ptr? (= 8 (bytevector-length (make-ptr-box))))

(when arch-64bit-ptr?
  (let ()
    (define x #x8877665544332211)    ;; 64bit
    (define p (integer->pointer x)) 
    (define i (pointer->integer p)) 
    (test-equal x i)))

(test-results)
