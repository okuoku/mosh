(library (nmosh zrepl platform posix)
         (export zrepl-interactive?
                 zrepl-input-subscribe
                 zrepl-input-acquire
                 zrepl-input-release
                 zrepl-output-width
                 zrepl-output-height
                 zrepl-zone-new
                 zrepl-fmt-open-line
                 zrepl-fmt-delete-line
                 zrepl-fmt-output
                 zrepl-fmt-set-cursor
                 )
         (import (rnrs)
                 (yuni core)
                 (nmosh stubs terminal) ;; Tentative..
                 (nmosh pffi posix fd)
                 (nmosh aio impl posix fd-ops)
                 (nmosh io master-queue)
                 (shorten)
                 (srfi :8)
                 (srfi :42)
                 (shorten))

;; FMT output
(define (zrepl-fmt-delete-line)
  (out 27 91 #\2 #\K "\r")
  )
(define (zrepl-fmt-open-line)
  (out "\r\n"))

(define (zrepl-fmt-output l)
  ;; FIXME: temp
  (define (out1 x)
    (for-each (^e (when (string? e)
                    (out e)
                    ))
              x))
  (define (out0 x)
    (for-each (^e 
                (cond
                  ((string? e) (out e))
                  ((pair? e) (out1 x))))
              x)) 
  (if (string? l)
    (out l)
    (out0 l)))

(define (zrepl-fmt-set-cursor x)
  (display "\r" (current-output-port))
  (do-ec (: i x)
         (out 27 91 #\C)))

;; TTY Related
(define (zrepl-interactive?)
  ;; We assume screen output is interactive when stdout was a TTY
  (not (= 0 (terminal_isatty 1))))
(define (zrepl-input-acquire)
  (terminal_acquire))
(define (zrepl-input-release)
  (terminal_release))
(define (zrepl-output-width)
  (terminal_getsize))
(define (zrepl-output-height)
  ;; FIXME: ....
  20)

(define (zrepl-input-subscribe cb)
  (define state 'ASCII) ;; => ASCII | ESCAPE | CSI | UTF8
  ;; cb = ^[Char ctrl? alt? super?]
  ;;      ^[sym ctrl? alt? super?]
  ;;      ^[#f obj]
  ;; sym = backspace | escape | up | down | right | left
  (define stdin (int->fd 1))
  (define (cook i)
    ;; Process a byte
    ;; FIXME: Decode UTF-8 here.
    (case state
      ((ASCII)
       (cond
         ((= i 27) ;; Escape byte
          ;; FIXME: Start escape timer here.
          (set! state 'ESCAPE))
         ((< i 32) 
          (cb (integer->char (+ i 64)) #t #f #f))
         (else
           (let ((c (integer->char i)))
             (cond
               ((char=? #\delete c)
                (cb 'backspace #f #f #f))
               (else
                 (cb c #f #f #f)))))))
      ((ESCAPE)
       (cond
         ((= i 91) ;; '[' = CSI
          (set! state 'CSI))
         (else
           ;; TEMP
           (cb 'escape #f #f #f)
           (set! state 'ASCII)
           (cook i))))
      ((CSI)
       (cond
         ((= i 65) ;; A
          (cb 'up #f #f #f)
          (set! state 'ASCII))
         ((= i 66)
          (cb 'down #f #f #f)
          (set! state 'ASCII))
         ((= i 67)
          (cb 'right #f #f #f)
          (set! state 'ASCII))
         ((= i 68)
          (cb 'left #f #f #f)
          (set! state 'ASCII))
         (else
           (cb 'CSI #f #f #f)
           (set! state 'ASCII)
           (cook i))))))
  (define (proc buf len)
    (do-ec (: i len)
           (cook (bytevector-u8-ref buf i))))
  (queue-read0
    nmosh-io-master-queue
    stdin
    (^[fd buf len]
      (cond
        (buf (proc buf len))
        (else (cb #f "Read Error"))))))

;; Easy output
(define stdout (int->fd 2))
(define (out . objs)
  (receive (port proc) (open-bytevector-output-port)
    (define (finish)
      (define bv (proc))
      (define len (bytevector-length bv))
      (fd_write stdout bv len))
    (define (outone obj)
      (cond
        ((string? obj)
         (put-bytevector port (string->utf8 obj)))
        ((char? obj)
         (outone (list->string (list obj))))
        ((integer? obj)
         (put-u8 port obj))
        (else
          (assertion-violation 'out
                               "Invalid sequence"
                               obj))))
    (for-each outone objs)
    (finish)))

(define* zzone (static-area cur-x cur-y))
(define (zrepl-zone-new) ;; => zzone
  (make zzone
        (static-area '())
        (cur-x 0)
        (cur-y 0)))

(define (zrepl-prepare-zone height) 
  (apply out (append (list-ec (: i height) "\n"))) 
  (do-ec (: i height) (out 27 91 #\A)))

)
