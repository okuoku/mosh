(library (nmosh ext wx notifications)
         (export wx-show-notification)
         (import (rnrs)
                 (nmosh pffi interface)
                 (nmosh stubs mosh_wx window)
                 (nmosh stubs mosh_wx notification))

(define (wx-show-notification icon title message)
  (define i (case icon
              ((INFO INFORMATION) wxICON_INFORMATION)
              (else wxICON_INFORMATION)))
  (define n (mwx_notification_create title message
                                     i))
  (mwx_notification_show n -1)
  (mwx_notification_destroy n))

)
