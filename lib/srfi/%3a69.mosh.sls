;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a69/basic-hash-tables.sls
(library (srfi :69)
         (export
             hash-by-identity
             string-ci-hash
             string-hash
             hash
             hash-table-merge!
             hash-table-copy
             hash-table->alist
             hash-table-fold
             hash-table-walk
             hash-table-values
             hash-table-keys
             hash-table-size
             hash-table-update!/default
             hash-table-update!
             hash-table-exists?
             hash-table-delete!
             hash-table-set!
             hash-table-ref/default
             hash-table-ref
             hash-table-hash-function
             hash-table-equivalence-function
             alist->hash-table
             hash-table?
             make-hash-table
         )
         (import
             (srfi :69 basic-hash-tables)
         )
) ;; library (srfi :69)
