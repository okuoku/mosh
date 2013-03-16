(library (nmosh debugger hooks main-thread)
         (export)
         (import 
           (mosh)
           (nmosh debugger core)
           (nmosh stubs moshvm-helper)
           (rnrs))
         
;; Main-thread hook
;; EXPERIMENTAL. Do not use.
         
(define (interrupt-handler trace)
  (display "INTERRUPT!\n")
  (debugger #f trace))

(define (init)
  (set-symbol-value! '%nmosh-interrupt interrupt-handler)
  (moshvm_keyboard_interrupt_enable))
         
(init)
)
