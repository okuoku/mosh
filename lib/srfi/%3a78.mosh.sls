;; this file is an alias-library.
;;  alias of:
;;   lib/srfi/%3a78/lightweight-testing.sls
(library (srfi :78)
         (export
             check-passed?
             check-reset!
             check-set-mode!
             check-report
             check-ec
             check
         )
         (import
             (srfi :78 lightweight-testing)
         )
) ;; library (srfi :78)
