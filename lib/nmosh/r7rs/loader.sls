(library (nmosh r7rs loader)
         ;; Export nothing
         (export)
         (import (rnrs)
                 ;(mosh pp)
                 (yuni util files)
                 (nmosh r7rs loader-body)
                 (nmosh r7rs condexpand-query)
                 (primitives 
                   ex:acc-add-file-dependency
                   ex:get-current-file-path
                   library-name->filename
                   nmosh:r7rs-converter-install!))

;; R7RS library converter

(define (adddep filename)
  ;(write (list 'R7RS-ADDDEP filename))(newline)
  (ex:acc-add-file-dependency filename))

(define (read-include-file filename-in)
  (define filename
    (let ((f (ex:get-current-file-path)))
     (and f
          (path-append (path-dirname f)
                       filename-in))))
  ;(write (list 'R7RS-INCLUDE filename-in '=> filename))(newline)
  (and filename
       (file-exists? filename)
       (begin 
         (adddep filename)
         (file->sexp-list filename)))) 

(define conv #f)

(define (make-r7rs-converter/debug condexpand-query read-include-file)
  (unless conv
    (set! conv (make-r7rs-converter condexpand-query read-include-file)))
  (lambda (e) (let ((x (conv e)))
               ;(pp (list 'R7RS-CONVERT x))
               x)))

(define (register!)
  (nmosh:r7rs-converter-install! (make-r7rs-converter/debug condexpand-query
                                                            read-include-file)))

;; Register converter
(register!))
