(library (nmosh startup)
         (export startup
                 build-id)
         (import (rnrs) 
                 (nmosh runlib)
                 (nmosh condition-printer)
                 (nmosh minidebug)
                 (nmosh global-flags)
                 (nmosh library-alias)
                 (primitives ca-load ca-preload-enable
                             ca-preload-core
                             ca-archive-enable
                             ca-archive-boot
                             DEBUGMODE-ON set-symbol-value!)
                 (for 
                   (primitives ex:unique-token)
                   expand))


(define-syntax define-build-id
  (lambda (stx)
    (syntax-case stx ()
      ((_ name)
       (with-syntax ((id (datum->syntax #'stx (ex:unique-token))))
         #'(define name id))))))

(define-build-id build-id)
(define (startup)
  (set-symbol-value! '%nmosh-failproc enter-debugger)
  (set-symbol-value! 'show-profile show-profile)
  (ca-archive-boot) ;; Set ca-base-libraries
  (init-library-alias-table)
  ;; Enable archive first. archive loader uses preload-core feature
  (let ((p (get-global-flag '%nmosh-archive-pointer))
        (s (get-global-flag '%nmosh-archive-size)))
    (when (and p s)
      (ca-archive-enable p s)))
  (when (get-global-flag '%nmosh-preload-core)
    (ca-preload-core build-id))
  (when (get-global-flag '%nmosh-preload-mode)
    (ca-preload-enable build-id))
  (let ((cl (command-line)))
    (cond
      ((<= 1 (length cl))
       (if (get-global-flag '%invoke-applet)
         (let ((name (string->symbol (car cl))))
           (runlib `((nmosh applet ,name)) name))
         (ca-load (car cl) #f '(nmosh PROGRAM-FROM-NMOSH-STARTUP))))
      ((get-global-flag '%nmosh-skymosh)
       (runlib '((nmosh skymosh)) 'skymosh))
      (else 
	(runlib '((nrepl simple)) 'nrepl)))))

(define (enter-debugger c trace)
  (define (fallback x)
    (display "Couldn't start script debugger, now using minidebug.\n" (current-error-port))
    (call-with-port (current-error-port) 
		    (lambda (p) (minidebug p c trace))))
  (display "Launching script debugger...\n" (current-error-port))
  (DEBUGMODE-ON)
  (set-symbol-value! '%nmosh-fail-condition c)
  (set-symbol-value! '%nmosh-fail-trace trace)
  (runlib/fallback fallback '((nmosh debugger)) 'debugger))

)
