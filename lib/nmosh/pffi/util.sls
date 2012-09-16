(library (nmosh pffi util)
         (export string->utf8/null
                 utf8/null->string
                 null-filter)
         (import
           (only 
             (mosh ffi)
             pointer?
             pointer->integer
             null-terminated-utf8->string)
           (rnrs))

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
