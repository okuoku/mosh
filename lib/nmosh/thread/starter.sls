;; FIXME: This library should be folded into (nmosh startup)
(library (nmosh thread starter)
         (export starter thread-create)
         (import (rnrs)
                 (rnrs eval)
                 ;(nmosh runlib)
                 (nmosh pffi interface)
                 (nmosh stubs moshvm-helper)
                 (nmosh global-flags))

(define (thread-create name lib func . params) ;; => VM
  (let ((vm (moshvm_alloc))
        (disable-acc? (get-global-flag '%disable-acc)))
    (moshvm_set_value_boolean vm "%disable-acc" (if disable-acc? 1 0))
    (moshvm_set_value_string vm "%invoke-applet" "nmosh-thread-starter")
    (moshvm_set_value_string vm "%nmosh-thread-func-name" (symbol->string func))
    (moshvm_set_value_pointer vm "%nmosh-thread-lib" (object->pointer lib))
    (moshvm_set_value_pointer vm "%nmosh-thread-params" (object->pointer
                                                          params))
    (moshvm_start vm 0 name)
    vm))

(define (starter)
  ;; Thread starter. Used by (nmosh applet nmosh-thread-starter) applet
  (let ((f (string->symbol (get-global-flag '%nmosh-thread-func-name)))
        (l (pointer->object (get-global-flag '%nmosh-thread-lib)))
        (p (pointer->object (get-global-flag '%nmosh-thread-params))))
    ;(write (list 'thread-start: f l p))(newline)
    (let ((func (eval f (environment l))))
      ;(write (list 'func: func))
      (apply func p))))

)
