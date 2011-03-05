;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a25/multi-dimensional-arrays.sls
(library (srfi :25)
         (export
             share-array
             array-set!
             array-ref
             array-end
             array-start
             array-rank
             array
             shape
             make-array
             array?
         )
         (import
             (srfi :25 multi-dimensional-arrays)
         )
) ;; library (srfi :25)
