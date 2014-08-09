(library (nmosh r7rs condexpand-syntax)
         (export cond-expand)
         (import (rnrs)
                 (for (nmosh r7rs condexpand-query) expand))

(define-syntax cond-expand
  (lambda (x)
    (syntax-case x (else)
      (()
       (syntax-violation #f "No matched cond-expand clause" x))
      (((clause body ...) next ...)
       (if (condexpand-query (lambda _ #f) (syntax->datum #'clause))
         #'(begin body ...)
         #'(cond-expand next ...))))))
         
         
)
