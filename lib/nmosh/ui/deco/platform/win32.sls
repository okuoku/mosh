(library (nmosh ui deco platform win32)
         (export deco
                 deco/err)
         (import (rnrs)
                 (shorten)
                 (srfi :8)
                 (yuni text decorate sexp)
                 (nmosh pffi win32 console))

;;

(define (color sym)
  (case sym
    ((white) 7)
    ((literal) 7)
    ((paren) 5)
    ((symbol) 14)
    (else 7)))

(define STDOUT #f)
(define STDERR #f)

(define (alloc-STDOUT!)
  (or STDOUT
      (set! STDOUT (win32_getstdhandle 1))))
(define (alloc-STDERR!)
  (or STDERR
      (set! STDERR (win32_getstdhandle 2))))

(define (sexp->string l)
  (receive (port proc) (open-string-output-port)
    (call-with-port port
                    (^p (write l p) (proc)))))

(define (deco-out h p l)
  (define (output e)
    (let ((sym (car e))
          (text (cadr e)))
      (win32_console_setcolor h (color sym) 0)
      (display text p)
      (win32_console_setcolor h (color 'white) 0)))
  (for-each
    (^e (for-each output e))
    l)
  )

(define (out-deco h p l)
  (define str (sexp->string l))
  (cond
    ((win32_console? h)
     (deco-out h p (sexp-decorate (list str))))
    (else (write l p))))



(define (deco l)
  (alloc-STDOUT!)
  (out-deco STDOUT (current-output-port) l))

(define (deco/err l)
  (alloc-STDERR!)
  (out-deco STDERR (current-error-port) l))

)
