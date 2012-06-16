(library (nmosh io serialport0)
         (export
           ;; Serial port
           start-serialport-talker)
         (import (rnrs)
                 (shorten)
                 (nmosh aio platform)
                 (nmosh io tcp0)
                 (nmosh io core)
                 (nmosh io master-queue))

;;

(define (start-serialport-talker device baud talker)
  ;; callback = ^fd
  (define s (queue-open-serialport0
              nmosh-io-master-queue
              device
              baud))
  (define (writer obj callback)
    (queue-write0 nmosh-io-master-queue
                  s
                  obj
                  callback))
  (define (read-callback fd buf len)
    (talker writer buf len))
  (cond
    (s
     (talker writer #t 0) ;; Notify
     (queue-read0 nmosh-io-master-queue s read-callback)
     #t)
    (else 
      (display (list 'FAIL: device))(newline)
      #f)))

)
