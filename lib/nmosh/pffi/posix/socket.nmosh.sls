(library (nmosh pffi posix socket)
         (export 
           ;; socket
           socket_getaddrinfo
           socket_create
           socket_freeaddrinfo
           socket_bind
           socket_listen
           socket_connect
           socket_accept
           socket_addrinfo_read
           socket_setnodelay
           inetname-port ;;FIXME
           inetname-family
           )
         (import (rnrs)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh pffi posix fd)
                 (system)
                 (srfi :8)
                 (prefix (nmosh stubs posix-socket) stub:))


(define null-pointer (integer->pointer 0))
(define (null-pointer? x) (= (pointer->integer x) 0))

;; socket

(define* addrinfo (ptr))

(define (pointer->addrinfo ptr)
  (make addrinfo (ptr ptr)))
(define* (addrinfo->pointer (addrinfo))
  (let-with addrinfo (ptr) ptr))

(define* inetname (sockaddr name len family port)) ;; FIXME: accept should return addr/port pair

(define (make-inetname ptr len)
  (let* ((bv (make-bytevector len))
         (bv-ptr (bytevector-pointer bv)))
    (pointer-copy! ptr bv-ptr len)
    (make-inetname/sockaddr bv)))

(define (make-inetname/sockaddr bv)
  (let ((addr-box (make-bytevector 16)) ;; Enough for IPv6
        (len-box (make-int-box))
        (family-box (make-int-box))
        (port-box (make-int-box)))
    (stub:socket_sockaddr_read bv family-box addr-box len-box port-box)
    (let ((family (int-box-ref family-box)))
      (case family
        ((4 6)
         (make inetname
               (family family)
               (sockaddr bv)
               (len (bytevector-length bv))
               (name addr-box)
               (port (int-box-ref port-box))))
        (else #f)))))

(define* (inetname-values (inetname))
  (let-with inetname (sockaddr len) (values sockaddr len)))

(define* (inetname-name (inetname))
  (receive (name len) (inetname-values inetname)
    name))

(define* (inetname-len (inetname))
  (receive (name len) (inetname-values inetname)
    len))

(define* (inetname-port (inetname))
  (let-with inetname (port) port))
(define* (inetname-family (inetname))
  (let-with inetname (family) family))

;; mode 0/4/6, proto 0/1(TCP)/2(UDP)
(define (socket_getaddrinfo name service mode proto)
  (let ((ret (make-ptr-box))
        (service (if (number? service) (number->string service) service)))
    (let ((r (stub:socket_getaddrinfo name service ret mode proto)))
      ;; FIXME: handle error
      (if (= 0 r)
        (pointer->addrinfo (ptr-box-ref ret))
        #f))))

(define* (socket_create mode proto)
  (let ((r (stub:socket_create mode proto)))
    (if (< 0 r)
      (int->fd r)
      #f)))

(define* (socket_freeaddrinfo (addrinfo))
  (stub:socket_freeaddrinfo (addrinfo->pointer addrinfo)))

(define* (socket_bind (fd) (inetname))
  (receive (sockaddr len) (inetname-values inetname)
    (stub:socket_bind (fd->int fd)
                      sockaddr
                      len)))

(define* (socket_listen (fd) n)
  (stub:socket_listen (fd->int fd) n))

(define* (socket_connect (fd) (inetname))
  (receive (sockaddr len) (inetname-values inetname)
    (stub:socket_connect (fd->int fd)
                         sockaddr
                         len)))

(define* (socket_accept (fd)) ;; => fd inetname / #f
  (let* ((len (stub:socket_sizeof_sockaddr_storage))
         (bv (make-bytevector len))
         (ret-box (make-int-box)))
    (int-box-set! ret-box len)
    (let ((r (stub:socket_accept (fd->int fd)
                                 bv
                                 ret-box)))
      (if (< r 1)
        (values #f #f)
        (values (int->fd r)
                (make-inetname/sockaddr bv))))))

(define* (socket_addrinfo_read (addrinfo)) ;; => inetname addrinfo/#f
  (let ((box-addr (make-ptr-box))
        (box-len (make-int-box))
        (box-next (make-ptr-box))
        (box-family (make-int-box)))
    (stub:socket_addrinfo_read (addrinfo->pointer addrinfo)
                               box-family
                               box-addr
                               box-len
                               box-next)
    (let ((addr (ptr-box-ref box-addr))
          (len (int-box-ref box-len))
          (next (ptr-box-ref box-next))
          (family (int-box-ref box-family)))
      (values
        (if (null-pointer? addr) #f (make-inetname addr len))
        (if (null-pointer? next) #f (pointer->addrinfo next))))))

(define* (socket_setnodelay (fd))
  (stub:socket_setnodelay (fd->int fd)))

)
