(library (nmosh thread sharedstorage)
         (export sharedstorage-set!
                 sharedstorage-cas!
                 sharedstorage-ref)
         (import (rnrs)
                 (nmosh pffi interface)
                 (nmosh stubs moshvm-helper)
                 (nmosh global-flags))

(define (sharedstorage-modify! proc)
  (define (toobj p)
    (and (not (= 0 (pointer->integer p)))
         (pointer->object p)))
  (define (exec ptr)
    ;; FIXME: Guard with dynamic-wind
    (define p (toobj (pointer-ref-c-pointer ptr 0)))
    (let ((r (proc p)))
      (unless (eq? r p)
        (pointer-set!-c-pointer! ptr 0 (object->pointer r)))))
  (define cb (make-callback exec))
  (moshvm_sharedstorage_get cb))

(define (htref in key)
  (and in
       (hashtable-ref ht key #f)))

(define (htset in key obj)
  (define (proc ht)
    (cond
      (obj
        (hashtable-set! ht key obj))
      (else
        (hashtable-delete! ht key))))
  (cond
    (in
      (proc in)
      ;; Return hashtable itself
      in)
    (else
      (let ((ht (make-hashtable equal-hash equal?)))
        (proc ht)
        ;; Return hashtable itself
        ht))))

(define (sharedstorage-set! key obj)
  (sharedstorage-modify!
    (lambda (in)
      (htset in key obj))))

(define (sharedstorage-ref key)
  (sharedstorage-modify!
    (lambda (in)
      (htref in key))))

(define (sharedstorage-cas! key obj objnew)
  (define result #f)
  (sharedstorage-modify!
    (lambda (in)
      (let ((r (htref in key)))
        (when (eq? r obj)
          (set! result #t)
          (htset in key obj)))))
  result)

)
