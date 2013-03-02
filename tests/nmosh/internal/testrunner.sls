(library (nmosh internal testrunner)
         (export testrunner)
         (import (rnrs)
                 (nmosh global-flags)
                 (mosh)
                 (primitives ca-load)
                 )

(define param (get-global-flag '%testrunner-param))

(define (testrunner dir arg)
  (set-current-directory! dir)
  (write (list 'running: arg))(newline)
  (ca-load arg #f '(nmosh TESTRUNNER)))

)
