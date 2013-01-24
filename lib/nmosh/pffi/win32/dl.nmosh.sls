(library (nmosh pffi win32 dl)
         (export win32_dl_lookup
                 win32_dl_open)
         (import (rnrs)
                 (nmosh pffi util)
                 (nmosh pffi win32 util)
                 (prefix (nmosh stubs win32-misc) stub:))

(define (win32_dl_lookup h name)
  (and h
       (let ((outname (string->utf8/null name)))
         (null-filter (stub:win32_dl_lookup h name)))))

(define (win32_dl_open name)
  (let ((outname (string->utf16-bv name)))
    (null-filter (stub:win32_dl_open outname))))
)
