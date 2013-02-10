(library (nmosh internal dll dllmain)
         (export dllmain)
         (import (rnrs)
                 (rnrs eval)
                 (mosh ffi) ;; pointer-set-c-pointer!
                 (nmosh ffi pffi)
                 (nmosh pffi interface)
                 (nmosh global-flags))

;; DLLMain for nmosh.dll

;;
(define (library-load libname)
  (display (list 'FIXME: "Unimplemented"))(newline))

(define (string->object str)
  (define p (open-string-input-port str))
  (read p))

(define (library-lookup libname objname)
  (define obj (eval (string->symbol objname)
                    (environment (string->object libname))))
  (write (list 'library-lookup: libname objname '=> obj))(newline)
  obj)

(define (dllmain)
  ;; DLL Entry point. Never return.
  (define dll-param (pointer->object (get-global-flag '%nmosh-dll-param)))
  (define dll-jmpbuf (get-global-flag '%nmosh-dll-jmpbuf))
  (define (fuse-table out in)
    (define (proc nam ptr)
      (define sym (string->symbol nam))
      (define fptr (cdr (assoc sym in)))
      (pointer-set-c-pointer! ptr 0 fptr))
    (for-each proc
              (map car out)
              (map cdr out)))
  ;(write (list 'dll-entrypoint: dll-param dll-jmpbuf))(newline)
  (fuse-table dll-param
              `((library-load . ,(object->pointer library-load))
                (library-lookup . ,(object->pointer library-lookup))))
  ;; Return to caller
  'ok)


)
