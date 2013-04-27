(library (chibi test)
         (export test-begin
                 test-end
                 test-values
                 test)
         (import (rnrs)
                 (nmosh condition-printer))

(define-syntax xguard
  (syntax-rules ()
    ((_ expected expr body)
     (begin
       (write (list 'TRY: 'expr '=> 'expected))(newline)
       (with-condition-printer
         body)))))

(define (test-begin . _) #f)
(define (test-end . _) #f)
(define-syntax test
  (syntax-rules ()
    ((_ expected expr)
     (xguard 
       expected
       expr
       (let ((res expr))
               (cond
                 ((not (equal? res expected))
                  (display "FAIL: ")
                  (write 'expr)
                  (display ": expected ")
                  (write expected)
                  (display " but got ")
                  (write res)
                  (newline))))))))

(define-syntax test-values
  (syntax-rules ()
    ((_ expected expr)
     (xguard 
       expected
       expr
       (call-with-values
         (lambda () expected)
         (lambda e*
           (call-with-values
             (lambda () expr)
             (lambda v*
               (cond
                 ((not (equal? v* e*))
                  (display "FAIL: ")
                  (write 'expr)
                  (display ": expected values ")
                  (write e*)
                  (display " but got values ")
                  (write v*)
                  (newline)))))))))))

)
