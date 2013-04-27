;; Copy/Pasted from r7rs-tests.scm
(library (chibi test)
         (export test-begin
                 test-end
                 test-values
                 test)
         (import (rnrs))

(define (test-begin . _) #f)
(define (test-end . _) #f)
(define-syntax test
  (syntax-rules ()
    ((_ expected expr)
     (let ((res expr))
       ((not (equal? expr expected))
        (display "FAIL: ")
        (write 'expr)
        (display ": expected ")
        (write expected)
        (display " but got ")
        (write res)
        (newline))))))

(define-syntax test-values
  (syntax-rules ()
    ((_ expected expr)
     (call-with-values
       (lambda () expected)
       (lambda e*
         (call-with-values
           (lambda () expr)
           (lambda v*
             ((not (equal? v* e*))
              (display "FAIL: ")
              (write 'expr)
              (display ": expected values ")
              (write e*)
              (display " but got values ")
              (write v*)
              (newline))    
             )))))))

)
