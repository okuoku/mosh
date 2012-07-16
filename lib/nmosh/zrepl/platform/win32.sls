(library (nmosh zrepl platform win32)
         (export zrepl-input-acquire
                 zrepl-input-release
                 zrepl-input-subscribe
                 zrepl-interactive?
                 )
         (import (rnrs)
                 (srfi :42)
                 (nmosh pffi win32 console)
                 (nmosh io master-queue)
                 (nmosh pffi win32 aio)
                 (nmosh aio impl win32 queue-iocp)
                 (prefix (nmosh stubs win32-misc) stub:)
                 (shorten))
;;

(define stdin (win32_getstdhandle 0))
(define stdout (win32_getstdhandle 1))
(define (zrepl-interactive?)
  (win32_console? stdout))

(define (zrepl-input-acquire)
  (win32_console_acquire stdin))
(define (zrepl-input-release)
  (win32_console_release stdin))

(define (zrepl-input-subscribe cb)
  ;; cb = ^[Char ctrl? alt? super?]
  ;;    = ^[sym ctrl? alt? super?]
  ;;    = ^[#f obj]
  (define (cook b)
    (display (list 'recv: b))(newline))
  (define (proc int-char int-ok?)
    (write (list 'char: int-char int-ok?))(newline)
    )
  ;; FIXME: Should be declared elsewhere
  (queue-invoke-ffithread nmosh-io-master-queue
                          (stub:win32_get_console_reader_func)
                          (handle->pointer stdin)
                          0
                          proc))
         
)
