(library (nmosh io pffi)
         (export pffi/async)
         (import (rnrs)
                 (srfi :8)
                 (srfi :42)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh ffi pffi)
                 (nmosh stubs pffi-stubs)
                 (nmosh aio platform)
                 (nmosh io master-queue))


;;

(define enqueue #f)
(define perform-enqueue enqueue)
(define pffi_caller (get_pffi_caller))

(define (packargs a b c d)
  (define (put port ptr)
    (define b (make-ptr-box))
    (ptr-box-set! ptr)
    (put-bytevector port b))
  (receive (p proc) (open-bytevector-output-port)
    (put p a)
    (put p b)
    (put p c)
    (put p d)
    (proc)))

;; Took from TRC shift/reset

;; Fixme: Allow null continuation
(define *mc* (lambda x 
               'nothing-to-do))
(define (set-mc c)
  (set! *mc* c))
(define (mc-ref) *mc*)

(define-syntax reset
  (syntax-rules ()
    ((_ body ...)
     (let ((mc (mc-ref)))
       (call/cc
         (lambda (k)
           (set-mc
             (lambda v
               (set-mc mc)
               (apply k v))) 
           (call-with-values
             (lambda () body ...)
             (mc-ref))))))))

(define-syntax shift
  (syntax-rules ()
    ((_ var body ...)
     (call/cc
       (lambda (k)
         (call-with-values
           (lambda ()
             (let ((var (lambda v (reset (apply k v)))))
               body ...)) 
           (mc-ref)))))))

(define-syntax pffi/async
  (syntax-rules ()
    ((_ body ...)
     (reset
       (let ()
         (define nullpo (integer->pointer 0))
         (define (proxy p ptr arg* ret)
           (shift k
                  (define p (packargs p ptr arg* ret)) 
                  (define (cb out0 out1)
                    (k))
                  (perform-enqueue pffi_caller p nullpo cb)))
         (pffi-call/proxy proxy (lambda () body ...)))))))

(define (fake-init)
  ;; FIXME: Initialize single worker
  (set! enqueue (queue-invoke-ffiqueue nmosh-io-master-queue)))

(fake-init)
)
