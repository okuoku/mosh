(library (nmosh ffi pffi-plugin platform win32)
         (export plugin-init
                 plugin-load)
         (import (rnrs)
                 (srfi :98)
                 (except (nmosh ffi pffi-lookup)
                         plugin-load)
                 (nmosh pffi win32 dl)
                 (nmosh pffi win32 env))

;;
;; FIXME:
(define (try-get-path)
  (let ((e (map (lambda (e)
                  (cons (string-upcase (car e))
                        (cdr e)))
                (get-environment-variables))))
    (let ((p (assoc "PATH" e)))
      (cdr p))))

;; FIXME: Use strings for function name..?
(define (plugin-lookup lib name)
  ;(write (list 'plugin-lookup: lib name))(newline)
  (win32_dl_lookup lib (symbol->string name)))
(define (plugin-load name)
  (win32_dl_open name))

(define (plugin-init plugin-path)
  ;; ... To break circular reference
  (set-pffi-plugin-loader! plugin-load plugin-lookup)
  ;; Add plugin path to PATH
  (setenv "PATH"
          (string-append plugin-path ";"
                         (try-get-path)))) 

)
