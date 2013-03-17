(library (nmosh tests test-threads)
         (export 
           thread1234/ret
           thread1234/immediate
           thread1234/100ms)
         (import 
           (rnrs)
           (only (mosh concurrent) sleep))

(define (thread1234/ret)
  1234)
(define (thread1234/immediate . bogus)
  (exit 1234))         
(define (thread1234/100ms . bogus)
  (sleep 100)
  (exit 1234))
         
)
