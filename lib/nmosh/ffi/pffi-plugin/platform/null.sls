(library (nmosh ffi pffi-plugin platform null)
         (export plugin-init
                 plugin-load
                 plugin-lookup)
         (import (rnrs)
                 (mosh ffi))
         
         
;; 
(define (plugin-init . args)
  'ok)

;; Use mosh variant for now
(define (plugin-load name)
  (open-shared-library name))
(define (plugin-lookup lib name)
  (lookup-shared-library lib name))
)
