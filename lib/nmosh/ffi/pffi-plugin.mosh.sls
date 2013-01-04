(library (nmosh ffi pffi-plugin)
         (export plugin-path
                 make-pffi-ref/plugin
                 pffi-c-function)
         (import (rnrs)
                 (mosh)
                 (mosh ffi)
                 (nmosh ffi pffi)
                 (nmosh ffi pffi-plugin platform)
                 (nmosh ffi pffi-ref)
                 (nmosh global-flags)
                 (yuni util files))


(define plugin-initialized? #f)

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

)
