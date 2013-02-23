;; Setup fake command line and parameters
(define %true-command-line (command-line))
(define fasl-name (cadr %true-command-line))
(define input-loadpath (caddr %true-command-line))
(define input-target (car (cdddr %true-command-line)))
(define (command-line) (cdr (cdddr %true-command-line)))
(display "fasl-name: ")
(write fasl-name)(newline)
(display "input-loadpath: ")
(write input-loadpath)(newline)
(display "input-target: ")
(write input-target)(newline)
(display "command-line:")
(write (command-line))(newline)(newline)
;(define %true-host-os (host-os))

;; Override the VM options
(define %disable-acc (if (and (string? input-target)
                              (string=? input-target "HOST"))
                       (host-os)
                       input-target))

(define %verbose #t)
(define %loadpath input-loadpath)

;; Load and run FASL
(define p (open-file-input-port fasl-name))
(define code (fasl-read p))

(display "Starting ")
(write fasl-name)
(display "...\n")

(eval-compiled! code)
