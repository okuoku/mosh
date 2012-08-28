(library (nmosh net msgpack)
         (export make-msgpack-client-socket
                 make-msgpack-server-socket
                 start-msgpack-talker)
         (import (rnrs)
                 (shorten)
                 (nmosh io tcp0)
                 (srfi :39)
                 (nmosh aio platform) ;; tcp-set-nodelay0
                 (yuni binary codec msgpack))

;;
(define (start-msgpack-talker fd recv-callback write-callback error-callback)
  ;; recv-callback = (^[obj] ...)
  ;; write-callback = (^[proc-write proc-close] ...)/#f
  ;; error-callback = (^[fd] ...)
  (let ((deser (make-msgpack-deserializer recv-callback)))
    (define (reader fd buf len)
      (if buf
        (deser (cons buf len))
        (error-callback fd)))

    (when write-callback
      (let ()  ;; why ??
        (define in-progress? #f)
        (define queue '())
        (define seg/queue '())
        (define seg/callback '())
        (define (close graceful-shutdown? cb)
          ;; FIXME: Implement graceful-shutdown
          (socket-close fd)
          (cb #f))
        (define (writer obj callback)
          (define (send-callback fd)
            (cond
              ((not fd)
               (error-callback fd))
              ((pair? seg/queue)
               (let ((sendseg (caar seg/queue)))
                 (set! seg/queue (cdr seg/queue))
                 (socket-write fd sendseg send-callback)))
              (else
                (let ((my-callback seg/callback))
                  (cond
                    ((pair? queue)
                     (set! seg/queue (generate-msgpack-buffer (caar queue))) 
                     (set! seg/callback (cdar queue)) 
                     (set! queue (cdr queue)) 
                     ;; Kick next
                     (send-callback fd))
                    (else 
                      (set! in-progress? #f)
                      (set! seg/callback #f)))
                  ;(display (list 'msgpack-call my-callback))(newline)
                  (my-callback)))))

          (cond
            (in-progress?
             (set! queue (append queue (list (cons 
                                               obj
                                               callback)))))
            (else
              (set! in-progress? #t)
              (set! seg/queue (generate-msgpack-buffer obj))
              ;(display (list 'msgpack-queue callback))(newline)
              (set! seg/callback callback)
              (send-callback fd))))

        (write-callback writer close))) 
    (start-read fd reader)))

;;  
(define (make-msgpack-server-socket name port 
                                    accept-callback 
                                    error-callback
                                    result-callback)
  ;; accept-callback = (^[servfd fd inetname] ...)
  ;; error-callback = (^[fd] ...)
  ;; result-callback = (^[success? inetname/message])
  (make-server-socket
    name port
    (^[servfd]
      (socket-accept servfd (^[fd inetname]
                              (tcp-set-nodelay0 fd)
                              (accept-callback servfd fd inetname))))
    result-callback))

;;  
(define (make-msgpack-client-socket name port recv-callback write-callback error-callback)

  (make-client-socket 
    name port 
    (^[fd] 
      (tcp-set-nodelay0 fd)
      (start-msgpack-talker fd recv-callback write-callback error-callback))))

)
