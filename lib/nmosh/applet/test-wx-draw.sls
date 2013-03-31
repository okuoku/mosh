(library (nmosh applet test-wx-draw)
         (export test-wx-draw)
         (import (rnrs)
                 (srfi :8)
                 (shorten)
                 (match)
                 (yuni core)
                 (nmosh ffi box)
                 (nmosh ffi pffi)
                 (nmosh pffi interface)
                 (nmosh stubs mosh_wx frame)
                 (nmosh stubs mosh_wx window)
                 (nmosh stubs mosh_wx graphics)
                 (nmosh ext wx main)) 

         
(define (window-getsize wnd)
  (define w (make-int-box))
  (define h (make-int-box))
  (mwx_window_getsize wnd w h)
  (values (int-box-ref w) (int-box-ref h)))
(define (window-refresh-all wnd)
  (receive (w h) (window-getsize wnd)
    (display (list 'refresh: w h))(newline)
    (mwx_window_refresh wnd 0 0 w h)))

(define-syntax drawop-elem
  (syntax-rules ()
    ((_ name op params ...)
     (list 'name op (quote (params ...))))))
(define-syntax defdrawops
  (syntax-rules ()
    ((_ name (ops ...) ...)
     (define name (list (drawop-elem ops ...) ...)))))
(defdrawops 
  drawops
  (path-begin MWX_PATH_BEGIN)
  (path-end MWX_PATH_END)
  (circle MWX_PATH_CIRCLE D D D)
  (fill-winding MWX_PATH_FILL_ODDEVEN)
  (rect MWX_PATH_RECT D D D D)
  (brush MWX_SET_BRUSH O))
(define white #f)
(define black #f)
(define (draw dc w h x y)
  (define (gen)
    `((path-begin)
      (brush ,black)
      (rect 0 0 ,w ,h)
      (fill-winding)
      (path-end)
      (path-begin)
      (brush ,white)
      (circle ,x ,y 20)
      (fill-winding)
      (path-end)))
  (define (pack lis)
    (define (lookup-op op)
      (let ((a (assq op drawops)))
        (cond
          (a (cdr a))
          (else (assertion-violation 'lookup-op
                                     "Unkown op"
                                     op)))))
    (define op-count 0)
    (define vtx-count 0)
    (define obj-count 0)
    (let-values 
      (((op-port op-proc) (open-bytevector-output-port))
       ((vtx-port vtx-proc) (open-bytevector-output-port))
       ((obj-port obj-proc) (open-bytevector-output-port)) )
      (define (fixnum i) ;; FIXME: VM bug workaround
        (bitwise-ior 0 i))
      (define (op i)
        (set! op-count (+ 1 op-count))
        (put-u8 op-port (fixnum i)))
      (define (vtx v)
        (define bv (make-bytevector 8))
        (bytevector-ieee-double-native-set! bv 0 v)
        (set! vtx-count (+ 1 vtx-count))
        (put-bytevector vtx-port bv))
      (define (obj o)
        (define bv (make-ptr-box))
        (ptr-box-set! bv o)
        (set! obj-count (+ 1 obj-count))
        (put-bytevector obj-port bv))
      (define (emit-arg type arg)
        (case type
          ((D) (vtx (inexact arg)))
          ((O) (obj arg))
          (else (assertion-violation 'emit-arg "Invalid type"
                                     type arg))))
      (define (emit-args types args)
        (when (pair? types)  ;; FIXME: for-each not allow '() '()....
          (for-each emit-arg types args)))
      (define (packone o)
        (define typ (lookup-op (car o)))
        (op (car typ))
        (emit-args (cadr typ) (cdr o)))
      (for-each packone lis)
      (op 0) ;; MWX_TERMINATE
      (values (op-proc) op-count (vtx-proc) vtx-count (obj-proc) obj-count)))

  (unless white
    (set! white (mwx_brush_create 255 255 255 255 wxBRUSHSTYLE_SOLID)))
  (unless black
    (set! black (mwx_brush_create 0 0 0 255 wxSOLID)))
  (receive (op op-cnt vtx vtx-cnt obj obj-cnt) (pack (gen))
    (mwx_graphics_kick_onpaint dc op op-cnt vtx vtx-cnt obj obj-cnt
                               NULL 0)))

(define (pos e)
  (list (mwx_event_mouse_x e)
        (mwx_event_mouse_y e)))
(define NULL (integer->pointer 0))
(define (main)
  (define frm)
  (define wnd)
  (define siz)
  (define p #f)
  (define (wnd-handler . e)
    (match e
           (("mouse" e)
            (set! p (pos e))
            (write (list 'mouse p))(newline)
            (window-refresh-all wnd))
           (("paint" e dc)
            (when p
              (receive (w h) (window-getsize wnd)
                (draw dc w h (car p) (cadr p))
                (display (list 'paint: w h))(newline))))
           (else
             (write (list 'wnd-handler e))(newline))))
  (define (frm-handler . e)
    (write (list 'frm-handler e))(newline)
    (match e
           (("close" _)
            (exit 0))
           (else 'ok)))
  (set! frm
    (mwx_frame_create (make-callback frm-handler)
                      "Nmosh draw test"
                      0 0 0 0
                      "Nmosh draw test2"
                      NULL
                      wxDEFAULT_FRAME_STYLE))
  (set! wnd
    (mwx_window_create_paintable (make-callback wnd-handler) frm 0))
  (set! siz (mwx_boxsizer_create wxVERTICAL))
  (mwx_sizer_add_window siz wnd 1 wxEXPAND)
  (mwx_window_setsizer frm siz)
  (mwx_window_show frm 1))
         
(define (test-wx-draw)
  (wx-main main))         
         
)
