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
  (define (flag? in x)
    (not (= 0 (bitwise-and in x))))
  (define (proc int-char int-type)
    (let ((ctrl? (flag? int-type 32))
          (alt? (flag? int-type 16))
          (vk? (flag? int-type 2))
          (unicode? (flag? int-type 1)))
      (define (sym s)
        ;; FIXME: Currently we cannot send ctrl/alt...
        (cb s ctrl? alt? #f))
      (cond
        (unicode?
          (let ((c (if ctrl? (+ int-char 64) int-char)))
            (cb (integer->char c) ctrl? alt? #f)))
        (vk?
          (case int-char
            ((37)
             (sym 'left))
            ((38)
             (sym 'up))
            ((39)
             (sym 'right))
            ((40)
             (sym 'down))
            ((16 17 18 91) ;; ignore shift/alt/...
             'ok)
            (else
              ;(write (list 'vk: int-char int-type))(newline)
              'ok))))))
  ;; FIXME: Should be declared elsewhere
  (queue-invoke-ffithread nmosh-io-master-queue
                          (stub:win32_get_console_reader_func)
                          (handle->pointer stdin)
                          0
                          proc))
         
)
