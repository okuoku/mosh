(library (nmosh aio impl posix posix-socket)
         (export
           queue-listen
           queue-accept
           queue-connect
           tcp-set-nodelay0
           resolve-socketname/4
           resolve-socketname/6

           inetname-port)
         (import (rnrs)
                 (shorten)
                 (srfi :8)
                 (nmosh pffi posix fd)
                 ;; FIXME; use aliased library for FreeBSD...
                 (nmosh aio impl posix queue-fd-poll)
                 (nmosh pffi posix socket))

;; Socket
(define (queue-connect Q name callback)
  ;; callback = ^[fd]
  (let* ((family (inetname-family name))
         (fd (socket_create family 1)))
    (let ((r (socket_connect fd name)))
      (case r
        ((0) ;; connected
         (callback fd))
        ((1) ;; queued
         (queue-register-fd/write Q fd
                                 (^[fd _]
                                   (queue-unregister-fd/write Q fd)
                                   (callback fd))))
        (else
          (fd_close fd)
          (callback #f)))))
  #t)

;; FIXME: Listen is not async yet...
(define (queue-listen Q name callback) ;; => inetname/#f
  (let* ((family (inetname-family name))
         (fd (socket_create family 1)))
    (socket_bind fd name)
    (let ((r (socket_listen fd 5)))
      (if (= r 0)
        (begin 
          (queue-register-fd/read Q fd (^[fd _] (callback fd)))
          (socket-inetname fd))
        #f))))

(define (queue-accept Q fd callback)
  (receive (new-fd inetname) (socket_accept fd)
    (callback new-fd inetname)))

(define (tcp-set-nodelay0 fd)
  (socket_setnodelay fd))

;; Resolve API
(define (resolve-socketname**/sync name service mode proto) ;; => (inetname ...)
  (define (addrinfo->list addrinfo)
    (if addrinfo
      (receive (inetname next) (socket_addrinfo_read addrinfo)
        (cons inetname
              (addrinfo->list next)))
      '()))
  (let ((addrinfo (socket_getaddrinfo name service mode proto)))
    (if addrinfo
      (let ((l (addrinfo->list addrinfo)))
        (socket_freeaddrinfo addrinfo) ;; Don't forget to do this..
        l)
      #f)))

;; FIXME:
(define (resolve-socketname** Q name service mode proto cb)
  (cb (resolve-socketname**/sync name service mode proto)))

(define (resolve-socketname/4 Q name service cb)
  (resolve-socketname** Q name service 4 0 cb))

(define (resolve-socketname/6 Q name service cb)
  (resolve-socketname** Q name service 6 0 cb))

#|
(define (resolve-socketname* name service)
  (resolve-socketname** name service 0 0))

(define (resolve-socketname/TCP name service)
  (resolve-socketname** name service 0 1))

(define (resolve-socketname/UDP name service)
  (resolve-socketname** name service 0 2))

(define (resolve-socketname/TCP4 name service)
  (resolve-socketname** name service 4 1))

(define (resolve-socketname/UDP4 name service)
  (resolve-socketname** name service 4 2))

(define (resolve-socketname/TCP6 name service)
  (resolve-socketname** name service 6 1))

(define (resolve-socketname/UDP6 name service)
  (resolve-socketname** name service 6 2))
|#

)
