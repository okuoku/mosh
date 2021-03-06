;; FIXME: This library should be folded into (nmosh startup)
(library (nmosh thread starter)
         (export starter thread-create thread-create/detached)
         (import (rnrs)
                 (rnrs eval)
                 ;(nmosh runlib)
                 (nmosh pffi interface)
                 (nmosh stubs moshvm-helper)
                 (nmosh global-flags))

(define (set-vm-parameters! vm lib func params)
  (let ((loadpath (get-global-flag '%loadpath))
        (verbose? (get-global-flag '%verbose))
        (disable-acc? (get-global-flag '%disable-acc))
        (portable-mode? (get-global-flag '%nmosh-portable-mode))
        (prefixless-mode? (get-global-flag '%nmosh-prefixless-mode)))
    (moshvm_set_value_string vm "%loadpath" (or loadpath ""))
    (moshvm_set_value_boolean vm "%verbose" (if verbose? 1 0))
    (moshvm_set_value_boolean vm "%disable-acc" (if disable-acc? 1 0))
    (moshvm_set_value_boolean vm "%nmosh-prefixless-mode" (if prefixless-mode? 
                                                            1 0))
    (moshvm_set_value_boolean vm "*command-line-args*" 0)
    (moshvm_set_value_boolean vm "%nmosh-portable-mode" (if portable-mode? 1 0))
    (moshvm_set_value_string vm "%invoke-applet" "nmosh-thread-starter")
    (moshvm_set_value_string vm "%nmosh-thread-func-name" (symbol->string func))
    (moshvm_set_value_pointer vm "%nmosh-thread-lib" (object->pointer lib))
    (moshvm_set_value_pointer vm "%nmosh-thread-params" (object->pointer
                                                          params))))

(define (thread-create name lib func . params) ;; => VM
  (let ((vm (moshvm_alloc)))
    (set-vm-parameters! vm lib func params)
    (moshvm_start vm 0 name)
    vm))

(define (thread-create/detached name func-ptr func-data lib func . params) ;; => VM
  (let ((vm (moshvm_alloc)))
    (set-vm-parameters! vm lib func params)
    (moshvm_start_detached vm 0 name func-ptr func-data)
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
