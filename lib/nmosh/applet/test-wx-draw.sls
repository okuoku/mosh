(library (nmosh applet test-wx-draw)
         (export test-wx-draw)
         (import (rnrs)
                 (shorten)
                 (match)
                 (yuni core)
                 (nmosh ffi pffi)
                 (nmosh pffi interface)
                 (nmosh stubs mosh_wx frame)
                 (nmosh stubs mosh_wx window)
                 (nmosh ext wx main)) 

(define NULL (integer->pointer 0))
(define (main)
  (define frm)
  (define wnd)
  (define siz)
  (define (wnd-handler . e)
    (write (list 'wnd-handler e))(newline))
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
