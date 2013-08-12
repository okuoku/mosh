(library (nmosh applet test-wx-frames)
         (export test-wx-frames)
         (import (rnrs)
                 (nmosh ext wx gadget listener)
                 (nmosh ext wx main)) 

(define (main)
  (define (cb obj)
    (write (list 'obj: obj))
    (when (eq? #f obj)
      (logger #f))
    (when (string? obj)
      (logger obj))
    (newline))
  (define logger (wx-listener-create cb))
  'ok
  (display "Leaving from main init..."))
         
(define (test-wx-frames)
  (display "Initializing...")
  (wx-main main)
  (display "Leaving..."))         
         
)
