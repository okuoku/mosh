(library (nmosh r7rs loader-body)
         (export make-r7rs-converter)
         (import (rnrs)
                 (match))

(define (complain message libname obj)
  (assertion-violation 'r7rs-library
                       message
                       libname
                       obj))

(define (do-merge libname decl* condexpand-query read-include-file0)
  (define imports* '())
  (define exports* '())
  (define bodies* '()) ;; Reversed
  (define merge append)
  (define (do-include fn)
    (define e (read-include-file fn))
    (set! bodies* (cons (cons 'begin e) bodies*)))
  (define (do-include-decls fn)
    (define e (read-include-file fn))
    (for-each step e))
  (define (condexpand-error obj)
    (complain "Invalid condexpand clause" libname obj))
  (define (do-ce-clause* ce*)
    (match ce*
      (((clause . body) . next)
       (if (condexpand-query condexpand-error clause)
         (for-each step body)
         (do-ce-clause* next)))
      (else #f)))
  (define (read-include-file fn)
    (or (read-include-file0 fn)
        (complain "Include file not found" libname fn)))

  (define (step e)
    (match e
           (('import . i*)
            (set! imports* (merge imports* i*)))
           (('export . e*)
            (set! exports* (merge exports* e*)))
           (('begin . body)
            (set! bodies* (cons e bodies*)) )
           (('include . f*)
            (for-each do-include f*))
           ;; FIXME: Implement include-ci
           (('include-library-declarations . f*)
            (for-each do-include-decls f*))
           (('cond-expand . ce*)
            (do-ce-clause* ce*))

           (else (complain "Invalid library decl." libname e))))
  (for-each step decl*)
  `(library ,libname 
            (export . ,exports*)
            (import . ,imports*)
            ,@(reverse bodies*)))

(define (do-expand exp condexpand-query read-include-file)
  (match exp
         (('define-library libname . decl*)
          (do-merge libname decl* condexpand-query read-include-file))
         (else
           (complain "Invalid define-library form" #f #f))))

(define (make-r7rs-converter condexpand-query read-include-file)
  (lambda (form) (do-expand form condexpand-query read-include-file)))         

)
