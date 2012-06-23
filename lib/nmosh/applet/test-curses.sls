(library (nmosh applet test-curses)
         (export test-curses)
         (import (rnrs)
                 (shorten)
                 (srfi :48)
                 (yuni async)
                 (nmosh io ext curses0)
                 (nmosh io core))

(define self #f)

(define (test-curses)
  (io-dispatch-loop)) 

(seq (=> screen-create (^[type arg0 arg1 arg2 arg3]
                         (screen-put self (format "~w\n" 
                                                  (list type arg0 arg1 arg2 arg3))))
         => screen)
     (set! self screen))
)
