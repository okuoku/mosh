(library (nmosh zrepl platform win32)
         (export zrepl-input-acquire
                 zrepl-input-release
                 zrepl-input-subscribe
                 zrepl-interactive?
                 zrepl-output-width
                 zrepl-output-height
                 zrepl-fmt-open-line
                 zrepl-fmt-delete-line
                 zrepl-fmt-output
                 zrepl-fmt-set-cursor
                 zrepl-fmt-cursor-hmove
                 )
         (import (rnrs)
                 (srfi :48)
                 (srfi :42)
                 (srfi :8)
                 (nmosh pffi win32 util)
                 (nmosh pffi win32 console)
                 (nmosh io master-queue)
                 (nmosh pffi win32 aio)
                 (nmosh aio impl win32 handle-ops)
                 (nmosh aio impl win32 queue-iocp)
                 (prefix (nmosh stubs win32-misc) stub:)
                 (shorten))
;;

(define stdin (win32_getstdhandle 0))
(define stdout (win32_getstdhandle 1))

(define fmt-bold? #f)
(define fmt-fgidx 0)
(define fmt-bgidx 0)

(define (fmt-attr-reset)
  ;; NB: We don't have to apply settings immediately
  (set! fmt-bold? #f)
  (set! fmt-fgidx 7)
  (set! fmt-bgidx 0))

(define (fmt-attr-apply)
  (define (i x)
    (if fmt-bold? (+ 8 x) x))
  (win32_console_setcolor stdout (i fmt-fgidx) (i fmt-bgidx)))

(define (fmt-attr-proc sym)
  (case sym
    ((reset) (fmt-attr-reset))
    ((black) (set! fmt-fgidx 0))
    ((red) (set! fmt-fgidx 4))
    ((green) (set! fmt-fgidx 2))
    ((yellow) (set! fmt-fgidx 6))
    ((blue) (set! fmt-fgidx 1))
    ((magenta) (set! fmt-fgidx 5))
    ((cyan) (set! fmt-fgidx 3))
    ((normal white) (set! fmt-fgidx 7))
    ((bg-black) (set! fmt-bgidx 0))
    ((bg-red) (set! fmt-bgidx 4))
    ((bg-green) (set! fmt-bgidx 2) )
    ((bg-yellow) (set! fmt-bgidx 6))
    ((bg-blue) (set! fmt-bgidx 1))
    ((bg-magenta) (set! fmt-bgidx 5))
    ((bg-cyan) (set! fmt-bgidx 3))
    ((bg-normal bg-white) (set! fmt-bgidx 7))
    ((bold) (set! fmt-bold? #t))
    ((no-bold) (set! fmt-bold? #f))
    (else 'ignore)))

(define (out . obj)
  (define (emit e)
    (cond
      ((or (symbol? e) (number? e))
       (fmt-attr-proc e))
      ((string? e)
       (fmt-attr-apply)
       (win32_console_output stdout e))
      ((char? e)
       (fmt-attr-apply)
       (emit (list->string (list e))))
      ((list? e)
       (for-each emit e))))
  (for-each emit obj))

(define (zrepl-interactive?)
  (win32_console? stdout))

(define (zrepl-input-acquire)
  (win32_console_acquire stdin))
(define (zrepl-input-release)
  (win32_console_release stdin))

(define (zrepl-output-height)
  (receive (w h x0 y0 x1 y1 cx cy) (win32_console_getsize stdout)
    (- y1 y0)))

(define (zrepl-output-width)
  (receive (w h x0 y0 x1 y1 cx cy) (win32_console_getsize stdout)
    w))

(define (zrepl-fmt-delete-line)
  (receive (w h x0 y0 x1 y1 cx cy) (win32_console_getsize stdout)
    (win32_console_setpos stdout 0 cy)
    (out (list->string (list-ec (: i (- w 1)) #\space)))
    (win32_console_setpos stdout cx cy)))

(define (zrepl-fmt-open-line)
  (out "\r\n")
  (zrepl-fmt-set-cursor 0))

(define (zrepl-fmt-output l)
  (fmt-attr-reset)
  (zrepl-fmt-set-cursor 0)
  (out l))

(define (zrepl-fmt-cursor-hmove d)
  (receive (w h x0 y0 x1 y1 cx cy) (win32_console_getsize stdout)
    (win32_console_setpos stdout cx (+ cy d))))

(define (zrepl-fmt-set-cursor x)
  (receive (w h x0 y0 x1 y1 cx cy) (win32_console_getsize stdout)
    (win32_console_setpos stdout x cy)))

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
      (define (ctrl c)
        (cb c #t alt? #f))
      (win32_console_settitle (format "~w ~w" int-char int-type))
      (cond
        ;; NB: Several ctrl combinations are re-mapped
        ((and unicode? (>= int-char 32))
          (let ((c (if ctrl? (+ int-char 64) int-char)))
            (cb (integer->char c) ctrl? alt? #f)))
        ((not (= int-char 0))
          (case int-char
            ((8)
             (sym 'backspace))
            ((13)
             (ctrl #\j))
            ((27)
             (sym 'escape))
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
              (let ((c (if ctrl? (+ int-char 64) int-char)))
                (cb (integer->char c) ctrl? alt? #f))))))))
  ;; FIXME: Should be declared elsewhere
  (queue-invoke-ffithread nmosh-io-master-queue
                          (stub:win32_get_console_reader_func)
                          (handle->pointer stdin)
                          0
                          proc))
         
)
