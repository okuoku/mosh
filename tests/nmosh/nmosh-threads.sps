(import (rnrs)
        (yuni async)
        (nmosh pffi interface)
        (nmosh thread starter)
        (nmosh io core)
        (nmosh io master-queue)
        (nmosh aio platform)
        (mosh test)
        (srfi :8)
        )

(define Q nmosh-io-master-queue)

(define (with/ticket proc cb)
  (define my-ticket #f)
  (receive (func ticket) (queue-ticket-create 
                           Q
                           (lambda (ptr)
                             (cb (pointer->object ptr))
                             (queue-ticket-dispose Q my-ticket))) 
    (set! my-ticket ticket)
    (proc func ticket)))


(define (test/imm return)
  (define (invoke func ticket)
    (thread-create/detached "Test thread" func ticket '(nmosh tests test-threads) 'thread1234/immediate))
  (seq (=> with/ticket invoke => res)
       (test-equal 1234 res)
       (return)))

(define (main)
  (seq (=> test/imm =>)
       (test-results)
       (exit 0))
  (io-dispatch-loop))

(main)

