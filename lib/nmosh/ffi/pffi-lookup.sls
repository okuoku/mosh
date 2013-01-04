(library (nmosh ffi pffi-lookup)
         (export make-pffi-ref
                 pffi-lookup
                 set-pffi-plugin-loader!
                 plugin-load)
         (import (rnrs)
                 (nmosh ffi pffi-ref)
                 (nmosh global-flags))
;;

(define (plugin-load name)
  ;(write (list 'plugin-load name))(newline)
  (proc-plugin-load name))
(define proc-plugin-load #f)
(define plugin-lookup #f)
(define (set-pffi-plugin-loader! loader lookup)
  (set! proc-plugin-load loader)
  (set! plugin-lookup lookup))
 
(define (pffi-slot obj)
  (and (pffi? obj) (cadr obj)))

(define (pffi-lookup/external lib func)
  (let ((r (plugin-lookup lib func)))
    ;(display (list 'lookup-result: r))(newline)
    r))
   
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
  ;(write (list 'pffi-lookup: lib func))(newline)
  (if (pffi? lib)
    (pffi-lookup/internal lib func)
    (pffi-lookup/external lib func)))

)
