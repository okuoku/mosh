(library (nmosh ffi pffi-plugin platform posix)
         (export plugin-init
                 plugin-load
                 plugin-lookup)
         (import (rnrs)
                 (only (nmosh ffi pffi-lookup)
                       set-pffi-plugin-loader!)
                 (mosh ffi))
         
         
;; 
(define (plugin-init . args)
  (set-pffi-plugin-loader! plugin-load plugin-lookup))

;; Use mosh variant for now
(define (plugin-load name)
  (open-shared-library name))
(define (plugin-lookup lib name)
  (lookup-shared-library lib name))
)
