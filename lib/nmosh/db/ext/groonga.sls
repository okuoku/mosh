(library (nmosh db ext groonga)
         (export 
           mgrn-create
           mgrn-request
           mgrn-db-create-local
           mgrn-db-open-local
           mgrn-dispose)
         (import (rnrs)
                 (shorten)
                 (yuni async)
                 (yuni core)
                 (nmosh io core)
                 (nmosh io master-queue)
                 (nmosh aio platform)
                 (nmosh pffi interface)
                 (nmosh stubs mosh-groonga))

;;

(define* mgrn-ctx (ctx ticket in-flight? current-cb request-queue))

(define Q nmosh-io-master-queue)
(define (mgrn-create) ;; => CTX
  (define me (make mgrn-ctx))
  (define (result-callback ptr len)
    (define bv (make-bytevector len))
    ;(display (list 'callback: ptr len))(newline)
    (pointer-copy! ptr (bytevector-pointer bv) len)
    (mgrn_result_free ptr)
    (let-with me (ctx current-cb request-queue)
      (current-cb (utf8->string bv))
      (cond
        ((null? request-queue)
         (~ me 'in-flight? := #f))
        (else
          (let ((next-req (caar request-queue))
                (next-cb (cdar request-queue))
                (rest (cdr request-queue)))
            (~ me 'current-cb := next-cb)
            (~ me 'request-queue := rest)
            (mgrn_request ctx next-req (bytevector-length next-req)))) )))
  (let ((callback (queue-ticket-callback Q))
        (ticket (queue-ticket-acquire Q result-callback)))
    (let ((ctx (mgrn_create callback ticket)))
      (~ me 'in-flight? := #f)
      (~ me 'request-queue := '())
      (~ me 'ctx := ctx)
      (~ me 'ticket := ticket)
      me)))

(define* (mgrn-db-create-local (mgrn-ctx) name)
  (let-with mgrn-ctx (ctx) 
    (mgrn_db_create_local ctx name)))

(define* (mgrn-db-open-local (mgrn-ctx) name)
  (let-with mgrn-ctx (ctx)
    (mgrn_db_open_local ctx name)))
(define* (mgrn-dispose (mgrn-ctx))
  (let-with mgrn-ctx (ctx ticket)
    (mgrn_dispose ctx)
    (queue-ticket-dispose Q ticket)))
(define* (mgrn-request (mgrn-ctx) req cb)
  (let-with mgrn-ctx (ctx in-flight? request-queue) 
    (define bv (string->utf8 req))
    (cond
      (in-flight?
        (~ mgrn-ctx 'request-queue := (append request-queue
                                              (list (cons bv cb)))))
      (else
        (~ mgrn-ctx 'in-flight? := #t)
        (~ mgrn-ctx 'current-cb := cb)
        (mgrn_request ctx bv (bytevector-length bv))))))

(mgrn_init) ;; FIXME: Defer?
)
