(library (nmosh ext wx events)
         (export event-dispatch else make-eventhandler)
         (import (rnrs)
                 (shorten)
                 (nmosh ffi pffi)
                 (nmosh pffi interface)
                 (nmosh stubs mosh_wx window)
                 (match))

(define event-ids-ht #f)

(define (event-ids-init)
  (set! event-ids-ht (make-eq-hashtable))
  (for-each (^e (match e ((namestr . value)
                          ;(display (list 'out: namestr value))(newline)
                          (hashtable-set! 
                            event-ids-ht
                            (string->symbol namestr) value))))
            (pointer->object (mwx_event_acquire_ids))))

(define (event-id-lookup sym)
  (unless event-ids-ht
    (event-ids-init))
  (let ((r (hashtable-ref event-ids-ht sym -1)))
    ;(display (list 'event: sym '=> r (hashtable-keys event-ids-ht)))(newline)
    r))

(define-syntax event-dispatch
  (syntax-rules (else)
    ((_ evt ((event-sym param ...) body ...) ... (else else-body ...))
     (let ((event-id (car evt))
           (event-data (cdr evt)))
       (cond
         ((= event-id (event-id-lookup 'event-sym))
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
