(library (nmosh ffi pffi-plugin)
         (export plugin-path
                 make-pffi-ref/plugin
                 pffi-c-function
                 plugin-initialize)
         (import (rnrs)
                 (mosh)
                 (mosh ffi)
                 (nmosh ffi pffi)
                 (only (nmosh ffi pffi-lookup)
                       ;; FIXME: Why do we have plugin-load on both?
                       pffi-lookup)
                 (nmosh ffi pffi-plugin platform)
                 (nmosh ffi pffi-ref)
                 (nmosh global-flags)
                 (yuni util files))


(define plugin-initialized? #f)

(define moshvm_callback_call
  (pffi-lookup
    (make-pffi-ref 'moshvm_helper)
    'moshvm_callback_call))
(define moshvm_export_object
  (pffi-lookup
    (make-pffi-ref 'moshvm_helper)
    'moshvm_export_object))

(define pffi-feature-set              
  (let ((f (get-global-flag '%get-pffi-feature-set)))
    (if f (f) '())))

(define (make-pffi-ref/plugin name)
  (unless plugin-initialized?
    (plugin-init (plugin-path))
    (set! plugin-initialized? #t))
  (let ((plib (assoc name pffi-feature-set)))
    (if plib
      (make-pffi-ref name) 
      (let* ((stdpath (plugin-path))
             (plugin-file (path-append stdpath (string-append
                                                 (symbol->string name)
                                                 ".mplg"))))
        (plugin-load plugin-file)))))

(define (plugin-path)
  (let ((f (get-global-flag '%nmosh-prefixless-mode)))
    (if f
      (let ((basepath (path-dirname (mosh-executable-path))))
        (path-append basepath "plugins"))
      (path-append (standard-library-path) "plugins"))))

(define (plugin-initialize library initname)
  (let ((initfunc (pffi-lookup library initname)))
    (cond
      (initfunc
        (let ((init (pffi-c-function/init library initname)))
          (display (list 'Initializing: initname))(newline)
          (init moshvm_export_object moshvm_callback_call)))
      (else
        ;; Without init function. Ignore here.
        'ignore)))
  'ok)

)
