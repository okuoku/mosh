(library (nmosh gui platform win32 clipboard)
         (export 
           make-clipboard-handler
           clipboard-set-text)
         (import (rnrs)
                 (shorten)
                 (yuni core)
                 (nmosh pffi win32 gui)
                 (nmosh pffi interface)
                 (nmosh aio platform win32)
                 (nmosh io master-queue))

(define (make-clipboard-handler cb)
  (define w (win32_window_alloc))
  (define (callback err bytes ovl key)
    (case (pointer->integer key)
      ((0) ;; create
       (cb (make-hwnd bytes w)))))
  (queue-window-register nmosh-io-master-queue
                         w
                         callback))

(define (clipboard-set-text hndl text)
  (win32_clipboard_text_set hndl text))

)
