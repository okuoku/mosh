(library (nmosh ffi pffi-plugin platform null)
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
  (assertion-violation 'plugin-load
                       "Plugin load is not supported on this arch"
                       name))
(define (plugin-lookup lib name)
  (assertion-violation 'plugin-lookup
                       "Plugin lookup is not supported on this arch"
                       lib
                       name))
)
