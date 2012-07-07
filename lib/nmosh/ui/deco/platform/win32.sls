(library (nmosh ui deco platform win32)
         (export deco
                 deco/err
                 make-deco
                 make-deco/err
                 deco-out
                 deco-colors
                 deco-width
                 deco-height
                 deco-set-palette!
                 deco-has-palette?
                 deco-ident)
         (import (rnrs)
                 (match)
                 (shorten)
                 (yuni core)
                 (srfi :8)
                 (yuni text decorate sexp)
                 (nmosh pffi win32 console))

(define has-palette? 'undecided)
(define STDOUT #f)
(define STDERR #f)

(define (alloc-STDOUT!)
  (or STDOUT
      (set! STDOUT (win32_getstdhandle 1))))
(define (alloc-STDERR!)
  (or STDERR
      (set! STDERR (win32_getstdhandle 2))))
;;
(define (make-deco) ;; => #f/win32-handle
  (alloc-STDOUT!)
  (and (win32_console? STDOUT)
       STDOUT))
(define (make-deco/err)
  (alloc-STDERR!)
  (and (win32_console? STDERR)
       STDERR))

(define (deco-colors h)
  (and h 16))
(define (deco-ident h)
  'win32-console)

(define (deco-has-palette? h) ;; => #f/rgb
  (if (eq? has-palette? 'undecided)
    (let ((r (deco-set-palette/default! h)))
      (set! has-palette? (if r 'rgb24 #f))
      has-palette?)
    has-palette?))

(define default-palette
  '(#x00000000 ;; 0
    #x00000080 ;; 1
    #x00008000 ;; 2
    #x00008080 ;; 3
    #x00800000 ;; 4
    #x00800080 ;; 5
    #x00808000 ;; 6
    #x00808080 ;; 7
    #x00000000 ;; 8?
    #x000000ff ;; 9
    #x0000ff00 ;; 10
    #x0000ffff ;; 11
    #x00ff0000 ;; 12
    #x00ff00ff ;; 13
    #x00ffff00 ;; 14
    #x00ffffff ;; 15
    ))

(define (deco-set-palette/default! h) ;; => boolean
  (deco-set-palette! h default-palette))

(define (deco-set-palette! h l)
  (win32_console_setpalette h l))

(define (deco-out h l)
  ;; ((fg bg str) ...)
  (define (out e)
    (match e
           ((fg bg str)
            (win32_console_setcolor h fg bg)
            (cond
              ((eq? STDOUT h)
               (display str (current-output-port)))
              ((eq? STDERR h)
               (display str (current-error-port)))))))
  (for-each out l)
  (win32_console_setcolor h 7 0))

(define (wh h) ;; => w h
  (receive (w H x0 y0 x1 y1 cx cy) (win32_console_getsize h)
    (values w (- y1 y0))))

(define (deco-width h)
  (and h
       (receive (W H) (wh h) W)))

(define (deco-height h)
  (and h
       (receive (W H) (wh h) H)))


;;

(define (color sym)
  (case sym
    ((white) 7)
    ((literal) 7)
    ((paren) 5)
    ((symbol) 14)
    (else 7)))


(define (sexp->string l)
  (receive (port proc) (open-string-output-port)
    (call-with-port port
                    (^p (write l p) (proc)))))

(define (deco-out0 h p l)
  (define (output e)
    (let ((sym (car e))
          (text (cadr e)))
      (win32_console_setcolor h (color sym) 0)
      (display text p)
      (win32_console_setcolor h (color 'white) 0)))
  (for-each
    (^e (for-each output e))
    l))

(define (out-deco h p l)
  (define str (sexp->string l))
  (cond
    ((win32_console? h)
     (deco-out0 h p (sexp-decorate (list str))))
    (else (write l p))))



(define (deco l)
  (alloc-STDOUT!)
  (out-deco STDOUT (current-output-port) l))

(define (deco/err l)
  (alloc-STDERR!)
  (out-deco STDERR (current-error-port) l))

)
