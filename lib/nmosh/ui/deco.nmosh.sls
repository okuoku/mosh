(library (nmosh ui deco)
         (export deco
                 deco/err
                 decol
                 decol/err)
         (import (rnrs)
                 (nmosh ui deco platform))

(define (decol l)
  (deco l)
  (newline (current-output-port)))

(define (decol/err l)
  (deco/err l)
  (newline (current-error-port)))

)
