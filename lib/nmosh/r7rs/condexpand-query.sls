(library (nmosh r7rs condexpand-query)
         (export condexpand-query)
         (import (rnrs))

(define *static-condexpand-symbols*
  '(r7rs
     else ;; FIXME: Check this..
     exact-closed
     ieee-float
     full-unicode
     ratios
     nmosh))         

(define (condexpand-sym sym)
  (member sym *static-condexpand-symbols*))

(define (condexpand-query err-k e)
  (cond
    ((pair? e)
     (let ((a (car e))
           (d (cdr e)))
       (case a
         ((library)
          ;; FIXME: Implement it
          #f)
         ((and)
          (if (pair? d)
            (let ((x (car d))
                  (y (cdr d)))
              (or (condexpand-query err-k x)
                  (condexpand-query err-k (cons 'and y))))
            #t))
         ((or)
          (if (pair? d)
            (let ((x (car d))
                  (y (cdr d)))
              (or (condexpand-query err-k x)
                  (condexpand-query err-k (cons 'or y))))
            #f))
         ((not)
          (not (condexpand-query err-k d)))
         (else
           (err-k e)))))
    ((symbol? e) (condexpand-sym e))
    (else
      (err-k e))))

)