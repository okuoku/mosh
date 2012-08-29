(library (nmosh aio impl posix thread-ops)
         (export queue-invoke-ffithread
                 queue-invoke-ffiqueue)
         (import (rnrs)
                 (srfi :8)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh pffi posix ffithread)
                 (nmosh pffi posix fd)
                 (nmosh aio impl posix queue-fd-poll)
                 (nmosh aio impl posix fd-ops))

(define (invoke fd cb)
  (define out0 (make-bytevector size-of-pointer))
  (define out1 (make-bytevector size-of-pointer))
  (define (check c)
    (unless (= c size-of-pointer)
      (assertion-violation 'check
                           "short read"
                           c)))
  (check (fd_read fd out0 size-of-pointer))
  (check (fd_read fd out1 size-of-pointer))
  (cb (pointer->integer (ptr-box-ref out0)) 
      (pointer->integer (ptr-box-ref out1))))

(define (procin obj)
  (let ((b (make-ptr-box)))
    (ptr-box-set! b (cond
                      ((pointer? obj) obj)
                      ((integer? obj) (integer->pointer obj))
                      (else 
                        (assertion-violation 'procin
                                             "Invalid argument"
                                             obj))))
    b))

(define (queue-invoke-ffiqueue Q) ;; => (enqueue func in0 in1) => (out0 out1)
  (define waiters '())
  (define nullpo (integer->pointer 0))
  (define (pop!)
    (when (null? waiters)
      (assertion-violation 'pop!
                           "something wrong"))
    (let ((c (car waiters)))
      (set! waiters (cdr waiters))
      c))
  (define (cleanup!)
    (unless (null? waiters)
      (let ((cb (pop!)))
        (cb #f #f)
        (cleanup!))))

  (receive (in-res out-res) (fd_pipe)
    (define (callback fd evt)
      (case evt
        ((READ READ+WRITE)
         (let ((cb (pop!)))
           (invoke in-res cb)))
        (else
          (cleanup!))))
   (receive (in-cmd out-cmd) (fd_pipe)
     (define (enqueue func in0 in1 cb)
       (define func-bv (procin (if func func nullpo)))
       (define in0-bv (procin in0))
       (define in1-bv (procin in1))
       (fd_write out-cmd func-bv size-of-pointer)
       (fd_write out-cmd in0-bv size-of-pointer)
       (fd_write out-cmd in1-bv size-of-pointer)
       (set! waiters (append waiters cb))
       #t)
    (ffiqueue-invoke (fd->int in-cmd) (fd->int out-res) 0)
    (queue-register-fd/read Q in-res callback)
    enqueue)))

(define (queue-invoke-ffithread Q func in0 in1 cb)
  (define (callback fd evt)
    (case evt
      ((READ READ+WRITE) (invoke fd cb))
      (else
        (assertion-violation 'queue-invoke-ffithread
                             "Invalid event for queue-invoke-ffithread"
                             evt
                             (fd->int fd)))))
  (receive (in out) (fd_pipe)
    (ffithread-invoke 
      (fd->int out) func (integer->pointer in0) (integer->pointer in1))
    (queue-register-fd/read Q in callback)))

)
