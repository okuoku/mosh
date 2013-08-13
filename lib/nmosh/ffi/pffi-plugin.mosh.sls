(library (nmosh ffi pffi-plugin)
         (export plugin-path
                 make-pffi-ref/plugin
                 pffi-c-function
                 plugin-initialize
                 pickup-export
                 )
         (import (rnrs)
                 (mosh)
                 (mosh ffi)
                 (nmosh ffi pffi)
                 (only (nmosh ffi pffi-lookup)
                       ;; FIXME: Why do we have plugin-load on both?
                       pffi-lookup)
                 (nmosh ffi pffi-plugin platform)
                 (nmosh ffi pffi-ref)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh global-flags)
                 (yuni util files))


(define plugin-initialized? #f)

(define moshvm_plugin_callback_fill
  (pffi-lookup
    (make-pffi-ref 'moshvm_helper)
    'moshvm_plugin_callback_fill))

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
  (define (gen-ht objs)
    (define ht (make-eq-hashtable))
    (define (enter obj)
      (let ((name (car obj))
            (dat (cdr obj)))
        ;; FIXME: Ignore plugin. This also disables exporting list/structure
        (cond
          ((pair? dat)
           (for-each enter dat))
          (else
            ;(display (list 'enter name dat))(newline)
            (hashtable-set! ht 
                            (string->symbol name)
                            dat)))))
    (for-each enter objs)
    ht)
  (let ((initfunc (pffi-lookup library initname)))
    (cond
      (initfunc
        (let ((init (pffi-c-function/init library initname))
              (objbox (make-ptr-box)))
          ;(display (list 'Initializing: initname))(newline)
          (init moshvm_plugin_callback_fill objbox)
          (let ((p (ptr-box-ref objbox)))
            (if (= 0 (pointer->integer p))
              '() ;; No constants
              (let ((objs (pointer->object p)))
                ;(write (list 'Constants: objs))(newline)
                (gen-ht objs))))))
      (else
        ;; Without init function. Ignore here.
        '()))))

(define (pickup-export name ht)
  ;(display (list 'pickup ht name))(newline)
  (cond
    ((null? ht)
     (display (list "WARNING: Invalid library symbol = " name))
     (newline)
     #f)
    (else
      (hashtable-ref ht name #f) )))

)
