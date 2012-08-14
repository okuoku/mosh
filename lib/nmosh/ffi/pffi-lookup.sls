(library (nmosh ffi pffi-lookup)
         (export make-pffi-ref
                 pffi-lookup)
         (import (rnrs)
                 (nmosh global-flags)
                 (primitives %ffi-lookup))
;;

(define pffi-mark '*pffi-reference*)
 
(define (make-pffi-ref slot)
  `(,pffi-mark ,slot))
 
(define (pffi-slot obj)
  (and (pffi? obj) (cadr obj)))
 
(define (pffi? x)
  (and (pair? x) (eq? pffi-mark (car x))))

(define (pffi-lookup/external lib func)
  (%ffi-lookup lib func))
   
(define pffi-feature-set              
  (let ((f (get-global-flag '%get-pffi-feature-set)))
    (if f (f) '())))

(define (pffi-lookup/internal lib func)
  (define (complain)
    (assertion-violation 'pffi-lookup
                         "PFFI function not avaliable"
                         func
                         (pffi-slot lib)))
  (let ((plib (assoc (pffi-slot lib) pffi-feature-set)))
    (unless plib (complain))
    (let ((pfn (assoc func (cdr plib))))
      (unless pfn (complain))
      (cdr pfn))))

(define (pffi-lookup lib func)
  (if (pffi? lib)
    (pffi-lookup/internal lib func)
    (pffi-lookup/external lib func)))

)
