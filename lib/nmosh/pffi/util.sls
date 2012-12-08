(library (nmosh pffi util)
         (export string->utf8/null
                 utf8/null->string
                 null-filter
                 buffer-pointer
                 buffer->bytevector)
         (import
           (nmosh pffi interface)
           (only 
             (mosh ffi)
             null-terminated-utf8->string)
           (rnrs))

(define (buffer->bytevector p off len)
  (define bv (make-bytevector len))
  (pointer-copy! (buffer-pointer p off)
                 (bytevector-pointer bv)
                 len)
  bv)

(define (buffer-pointer p off)
  (let ((o (pointer->integer (if (bytevector? p) (bytevector-pointer p) p))))
    (integer->pointer (+ off o))))

(define (null-filter p)
  (and (pointer? p)
       (not (= 0 (pointer->integer p)))
       p))

(define (string->utf8/null x)
  (let* ((bv (string->utf8 x))
         (len (bytevector-length bv)))
    (let ((r (make-bytevector (+ 1 len) 0)))
      (bytevector-copy! bv 0 r 0 len)
      r)))

(define (utf8/null->string x)
  (null-terminated-utf8->string x))

)
