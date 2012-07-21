(library (nmosh applet turbocharge)
         (export turbocharge)
         (import (rnrs)
                 (only (srfi :1) lset-intersection)
                 (srfi :48)
                 (srfi :26)
                 (srfi :8)
                 (mosh pp)
                 (mosh file)
                 (nmosh global-flags)
                 (only (yuni util files)
                       file->bytevector)
                 (shorten)
                 (primitives
                   library-name->filename
                   CACHEPATH
                   ca-load/cache
                   ca-preload-disable
                   ca-preload-path
                   ca-preload-core-path
                   ca-filename->cachename
                   cachename-shorten
                   fasl-read
                   fasl-write
                   ))

(define library-core '(nmosh applet turbocharge core))
(define library-user '(nmosh applet turbocharge list))

(define (calc-cachepath libname)
  (define fn (library-name->filename libname))
  (and fn
       (string-append CACHEPATH (cachename-shorten 
                                  (ca-filename->cachename fn)))))

(define library-ht/core (make-hashtable equal-hash equal?))
(define library-ht/user (make-hashtable equal-hash equal?))

(define (phase1 set)
  (for-each (^e 
              (define fn (library-name->filename e))
              (format #t "Loading: ~a ~a\n" fn e)
              (ca-load/cache fn #f e))
            set))

(define (check-loaded/ht ht libname) (hashtable-ref ht libname #f))

(define (add-deps ht libname)
  (define filename (and (not (check-loaded/ht ht libname))
                        (calc-cachepath libname)))
  (define cacheport (and 
                      filename
                      (file-exists? filename)
                      (open-file-input-port filename)))
  (when cacheport

    (let* ((obj (fasl-read cacheport))
           (deps (assq 'DEPS obj)))
      (for-each (^e (and (pair? e) (add-deps ht (car e))))
                deps))
    (hashtable-set! ht libname filename)
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

(define (phase2/ht name ht ht-prev? set)
  (define delset (and ht-prev? (vector->list (hashtable-keys ht-prev?))))
  (for-each (cut add-deps ht <>) set)
  (when delset
    ;; Delete entries in ht-prev
    (for-each
      (^e (hashtable-delete! ht e))
      (lset-intersection equal?
                         (vector->list (hashtable-keys ht))
                         delset)))
  (receive (key val) (hashtable-entries ht)
    (receive (idx bv) (gen (vector->list key)
                           (vector->list val))
      (let ((p (create name)))
        (fasl-write idx p)
        (put-bytevector p bv)
        (close-port p))))) 

(define (turbocharge)
  (ca-preload-disable)

  ;; First, load-up entire system
  (phase1 (list library-core library-user))

  ;; core
  ;; Don't update the image if preload-core and file exists
  (unless (and (get-global-flag '%nmosh-preload-core)
               (file-exists? (ca-preload-core-path)))
   (phase2/ht (ca-preload-core-path) library-ht/core #f (list library-core)))

  ;; App
  (phase2/ht (ca-preload-path) library-ht/user library-ht/core 
             (list library-user)))

)
