(library (nmosh applet swank)
         (export swank)
         (import (rnrs) (nmosh swank))

(define (swank)
  (start-server #f)))
