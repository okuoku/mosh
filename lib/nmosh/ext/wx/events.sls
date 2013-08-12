(library (nmosh ext wx events)
         (export event-dispatch else make-eventhandler)
         (import (rnrs)
                 (nmosh ffi pffi)
                 (nmosh stubs mosh_wx window)
                 (match))

(define-syntax event-dispatch
  (syntax-rules (else)
    ((_ evt ((event-sym param ...) body ...) ... (else else-body ...))
     (let ((event-id (car evt))
           (event-data (cdr evt)))
       (cond
         ((= event-id event-sym)
          (match event-data
                 ((param ...)
                  body ...)))
         ...
         (else
           else-body ...))))
    ((_ evt clauses ...)
     (event-dispatch evt clauses ... (else #f)))))

(define (make-eventhandler proc)
  (define (evh id evt . params)
    (let ((res (apply proc id evt params)))
      (unless res
        (mwx_event_skip evt))
      res))
  (make-callback evh))

)
