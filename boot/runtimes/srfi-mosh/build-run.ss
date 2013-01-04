(import (rnrs)
	(mosh) (system)
	(rnrs eval)
	(primitives throw ex:destructive-eval! ex:current-environment sexp-map sexp-map/debug))

(define initprog
  '((import (rnrs) 
	    (mosh)
	    (system)
	    (rnrs mutable-pairs)
	    (primitives 
              %disable-acc
              permissive-eval sexp-map sexp-map/debug) (nmosh bootstrap util))
    (define %loadpath "lib.boot:lib.rnrs:lib:../../../lib")
    (define %nmosh-portable-mode #f)
    (define %nmosh-prefixless-mode #t)
    (define %verbose #t)
    (define %nmosh-failproc #f)
    (define (%getpid) 0)))

(define m (ex:current-environment))

(define (read-all/port p)
  (let ((r (read p)))
    (if (eof-object? r)
      '()
      (cons r (read-all/port p)))))

(define (read-all fn)
  (call-with-input-file fn read-all/port))
(define (xeval e)
  (ex:destructive-eval! e m))
(define (xload l)
  (for-each xeval l))

(define (pload fn)
  (display "loading ")
  (display fn)(newline)
  (xload (read-all fn))
  (display "done.")(newline))

(set-symbol-value! 'permissive-eval xeval)

(xload initprog)
(xeval '(define eval-core permissive-eval))
(pload "mosh-exceptions.scm")
(pload "compat-mosh-run.scm")
(pload "mosh-utils5.scm")
(pload "runtime.scm")
(xeval '(set! id-symbol '*ID-BUILD*))
(pload "expander.scm")
(pload "bootstrap.common/build-expander.scm")
(pload "layout.scm")
(pload "bootstrap.common/build-body.scm")
