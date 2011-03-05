;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a45/lazy.sls
(library (srfi :45)
         (export
             eager
             force
             lazy
             delay
         )
         (import
             (srfi :45 lazy)
         )
) ;; library (srfi :45)
