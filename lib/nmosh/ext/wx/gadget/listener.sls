(library (nmosh ext wx gadget listener)
         (export wx-listener-create)
         (import (rnrs)
                 (match)
                 (yuni core)
                 (nmosh ext wx events)
                 (nmosh ffi pffi)
                 (nmosh pffi interface)
                 (nmosh stubs mosh_wx fonts)
                 (nmosh stubs mosh_wx menu)
                 (nmosh stubs mosh_wx frame)
                 (nmosh stubs mosh_wx window)
                 (nmosh stubs mosh_wx controls))

(define (apply-monospace w)
  (define font (mwx_font_create 10 wxFONTFAMILY_TELETYPE 0 
                                wxFONTWEIGHT_NORMAL
                                0
                                "" wxFONTENCODING_SYSTEM))
  (define attr (mwx_textattr_create))
  (mwx_textattr_setfont attr font)
  (mwx_textctrl_setdefaultstyle w attr)
  (mwx_textattr_destroy attr)
  (mwx_font_destroy font))

(define* (wx-listener-create
           #((menu: '())
             (hint: "")
             (title: "NMosh Listener")
             (status: #f))
           cb) ;; => logger
  (define NULL (integer->pointer 0))
  (define frm #f)
  (define logarea #f)
  (define inputarea #f)
  (define siz #f)
  (define (logger . e)
    (match e
           ((#f) ;; Close
            (mwx_window_destroy frm))
           ((str)
            (mwx_textctrl_appendtext logarea str)
            (let ((l (mwx_textctrl_getlastposition logarea)))
              (mwx_textctrl_showposition logarea l)))))
  (define (inputarea-handler . e)
    (display (list 'inputarea-handler: e))(newline)
    (event-dispatch
      e
      ((wxEVT_COMMAND_TEXT_ENTER evt id)
       (let ((obj (pointer->object (mwx_textctrl_getvalue inputarea))))
         (and (list? obj)
              (begin 
                ;; Clear textarea
                (mwx_textctrl_setvalue inputarea "")
                ;; Send string
                (cb (utf8->string (car obj)))))))))

  (define (logarea-handler . e)
    ;(display (list 'logarea: e))(newline)
    #f)
  (define (frm-handler . e)
    (display (list 'frm-handler: e))(newline)
    (event-dispatch 
      e
      ((wxEVT_CLOSE_WINDOW _)
       (cb #f) #f)))
  (set! frm
    (mwx_frame_create (make-eventhandler frm-handler)
                      title:
                      0 0 0 0
                      "NMoshListener"
                      NULL
                      wxDEFAULT_FRAME_STYLE))
  (set! inputarea 
    (mwx_textctrl_create frm
                         (make-eventhandler inputarea-handler)
                         -1
                         (bitwise-ior wxTE_PROCESS_ENTER)) )
  (set! logarea
    (mwx_textctrl_create frm
                         (make-eventhandler logarea-handler)
                         -1
                         (bitwise-ior
                           wxTE_MULTILINE
                           wxTE_READONLY)))
  (set! siz (mwx_boxsizer_create wxVERTICAL))
  (mwx_sizer_add_window siz logarea 1 wxEXPAND)
  (apply-monospace logarea)
  (mwx_sizer_add_window siz inputarea 0 wxEXPAND)
  (mwx_textctrl_sethint inputarea hint:)
  (mwx_window_setsizer frm siz)
  (mwx_window_show frm 1)
  logger)

)
