
;;------------------------------------------------
;; error handling
;;------------------------------------------------

; from library.scm

; overrides std raise
(define (raise c)
  (cond ((current-exception-handler)
	 => (lambda (proc)
	      (proc c)
	      (cond ((parent-exception-handler)
		     => (lambda (proc)
			  (let ((create-non-continuable-violation (symbol-value 'create-non-continuable-violation)))
			    (if create-non-continuable-violation
			      (proc (create-non-continuable-violation c))
			      (display "      create-non-continuable-violation is not set\n" (current-error-port)))))))
	      (throw "     error in raise(NMOSH): returned from non-continuable exception\n"))))
  (if %nmosh-failproc
    (%nmosh-failproc c (%get-stack-trace-obj))
    (throw (format "    error in raise(NMOSH): unhandled exception has occurred (and (nmosh startup) was not loaded)\n\n~a\n" 
                   ;(condition-printer c (current-error-port))
                   c
                   ))))

;;------------------------------------------------
;; error reporting
;;------------------------------------------------

;from Larceny
(define (raise-syntax-violation form subform who message syntax-form syntax-subform trace)
  (let* ((c0 (make-who-condition who))
	 (c1 (make-message-condition message))
	 (c2 (make-syntax-violation form subform))
	 (c3 (make-syntax-trace-condition syntax-form syntax-subform trace)) ; MOSH
	 (c (if who (condition c0 c1 c2 c3) (condition c1 c2 c3))))
    (raise c)))

