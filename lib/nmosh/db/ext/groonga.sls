(library (nmosh db ext groonga)
         (export 
           mgrn-create
           mgrn-request
           mgrn-db-create-local
           mgrn-db-open-local
           mgrn-db-dispose)
         (import (rnrs)
                 (shorten)
                 (yuni async)
                 (nmosh io core)
                 (nmosh io master-queue)
                 (nmosh aio platform)
                 (nmosh pffi interface)
                 (nmosh stubs mosh-groonga))

(define Q nmosh-io-master-queue)
(define (mgrn-create cb) ;; => ctx
  (define (callback ptr len)
    (define bv (make-bytevector len))
    (pointer-copy! ptr (bytevector-pointer bv) len)
    (mgrn_result_free ptr)
    (cb bv))
  (let ((callback (queue-ticket-callback Q))
        (ticket (queue-ticket-acquire Q callback)))
    (mgrn_create callback ticket)))
(define (mgrn-db-create-local ctx name)
  (mgrn_db_create_local ctx name))
(define (mgrn-db-open-local ctx name)
  (mgrn_db_open_local ctx name))
(define (mgrn-db-dispose ctx)
  ;; FIXME: Dispose ticket here!
  (mgrn_dispose ctx))
(define (mgrn-request ctx req)
  (define bv (string->utf8 req))
  (mgrn_request ctx bv (bytevector-length bv)))


(mgrn_init) ;; FIXME: Defer?
)
