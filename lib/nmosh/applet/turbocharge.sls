(library (nmosh applet turbocharge)
         (export turbocharge)
         (import (rnrs)
                 (srfi :48)
                 (srfi :8)
                 (mosh pp)
                 (mosh file)
                 (only (yuni util files)
                       file->bytevector)
                 (shorten)
                 (primitives
                   library-name->filename
                   CACHEPATH
                   ca-load/cache
                   ca-preload-disable
                   ca-preload-path
                   ca-filename->cachename
                   cachename-shorten
                   fasl-read
                   fasl-write
                   ))

(define library-set
  '((nmosh applet turbocharge list)))

(define (calc-cachepath libname)
  (define fn (library-name->filename libname))
  (and fn
       (string-append CACHEPATH (cachename-shorten 
                                  (ca-filename->cachename fn)))))

(define library-ht (make-hashtable equal-hash equal?))

(define (phase1 set)
  (for-each (^e 
              (define fn (library-name->filename e))
              (format #t "Loading: ~a ~a\n" fn e)
              (ca-load/cache fn #f e))
            library-set))

(define (check-loaded libname) (hashtable-ref library-ht libname #f))

(define (add-deps libname)
  (define filename (and (not (check-loaded libname))
                        (calc-cachepath libname)))
  (define cacheport (and 
                      filename
                      (file-exists? filename)
                      (open-file-input-port filename)))
  (when cacheport

    (let* ((obj (fasl-read cacheport))
           (deps (assq 'DEPS obj)))
      (for-each (^e (and (pair? e) (add-deps (car e))))
                deps))
    (hashtable-set! library-ht libname filename)
    (format #t "Load: ~a\n" libname)
    (close-port cacheport))
  #|
  (unless cacheport
    (format #t "Skip: ~a\n" libname))
  |#
  )

(define (file-concat l)
  (receive (p out) (open-bytevector-output-port)
    (for-each (^e (put-bytevector p 
                                  (file->bytevector e)))
              l)
    (out)))

(define (create fn)
  (when (file-exists? fn)
    (delete-file fn))
  (open-file-output-port fn))

(define (create-index l)
  (define (itr off cur)
    (if (pair? cur)
      (let ((next (+ off (car cur))))
        (cons off 
              (itr next (cdr cur))))
      '()))
  (itr 0 l))

(define (gen libname* filename*) ;; => idx bv
  (define size* (map (^n (file-size-in-bytes n)) filename*))
  (define bv (file-concat filename*))
  (values (map cons libname* (create-index size*)) bv))

(define (phase2 set)
  (for-each add-deps set)
  (receive (key val) (hashtable-entries library-ht)
    (receive (idx bv) (gen (vector->list key)
                           (vector->list val))
      (let ((p (create (ca-preload-path))))
        (fasl-write idx p)
        (put-bytevector p bv)
        (close-port p))))) 

(define (turbocharge)
  (ca-preload-disable)
  (phase1 library-set)
  (phase2 library-set)
  )

)
