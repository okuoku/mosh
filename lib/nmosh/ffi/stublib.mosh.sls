(library (nmosh ffi stublib)
         (export define-ffi-library)
         (import (rnrs)
                 (mosh ffi)
                 (nmosh ffi pffi-lookup))

(define soext 
  (cond
    (on-darwin ".dylib")
    (else ".so")))

(define (soname basename)
  (string-append "lib" basename soext))

(define (search-and-open-shared-libary spec libname)
  (let ((name (soname (symbol->string spec))))
    (let ((so (plugin-load name)))
      so)))

(define-syntax define-ffi-library
  (syntax-rules ()
    ((_ name spec libname)
     (define name (search-and-open-shared-libary 'spec 'libname)))))

)
