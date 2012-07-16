(library (nmosh pffi win32 console)
         (export
           win32_console?
           win32_console_getpalette
           win32_console_setpalette
           win32_console_getsize
           win32_console_setpos
           win32_console_settitle
           win32_console_setcolor
           win32_getstdhandle
           win32_console_acquire
           win32_console_release)
         (import (rnrs)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh pffi util)
                 (nmosh pffi win32 aio)
                 (nmosh pffi win32 util)
                 (srfi :42)
                 (prefix (nmosh stubs win32-misc) stub:))

(define* (win32_console? (h win32-handle))
  (let ((r (stub:win32_console_p (handle->pointer h))))
    (not (= r 0))))

(define (win32_getstdhandle fd)
  (let ((r (stub:win32_getstdhandle fd)))
    (pointer->handle r)))

(define* (win32_console_getsize (ha win32-handle)) ;; => w h x0 y0 x1 y1 cx cy
  (let ((w (make-int-box))
        (h (make-int-box))
        (x0 (make-int-box))
        (y0 (make-int-box))
        (x1 (make-int-box))
        (y1 (make-int-box))
        (cx (make-int-box))
        (cy (make-int-box)))
    (stub:win32_console_getsize
      (handle->pointer ha)
      w h x0 y0 x1 y1 cx cy)
    (values
      (int-box-ref w)
      (int-box-ref h)
      (int-box-ref x0)
      (int-box-ref y0)
      (int-box-ref x1)
      (int-box-ref y1)
      (int-box-ref cx)
      (int-box-ref cy))))

(define* (win32_console_setpos (h win32-handle) x y)
  (stub:win32_console_setpos (handle->pointer h)
                             x y))

(define (win32_console_settitle title)
  (stub:win32_console_settitle (string->utf16-bv title)))

(define* (win32_console_setcolor (h win32-handle) fg bg)
  (stub:win32_console_setcolor (handle->pointer h)
                               fg
                               bg))

(define* (win32_console_setpalette (h win32-handle) l)
  (define pal (make-bytevector (* 4 16)))
  (do-ec (: i 16)
         (bytevector-u32-native-set! pal (* 4 i) (list-ref l i)))
  (not (= 0 (stub:win32_console_setpalette (handle->pointer h)
                                           pal))))

(define* (win32_console_getpalette (h win32-handle)) ;; => #f / l
  (define pal (make-bytevector (* 4 16)))
  (and (stub:win32_console_getpalette (handle->pointer h) pal)
       (list-ec (: i 16)
                (bytevector-u32-native-ref pal (* 4 i)))))

(define* (win32_console_acquire (h win32-handle))
  (stub:win32_console_acquire (handle->pointer h)))
(define* (win32_console_release (h win32-handle))
  (stub:win32_console_release (handle->pointer h)))


)
