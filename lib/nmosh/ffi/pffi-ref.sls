(library (nmosh ffi pffi-ref)
         (export make-pffi-ref pffi?)
         (import (rnrs))

(define pffi-mark '*pffi-reference*)
(define (make-pffi-ref slot)
  `(,pffi-mark ,slot))

(define (pffi? x)          
  (and (pair? x) (eq? pffi-mark (car x))))
)
