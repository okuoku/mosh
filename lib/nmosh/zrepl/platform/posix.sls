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
                 zrepl-fmt-cursor-hmove
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

;; FMT states
(define fmt-prop #f)
(define underline? #f)
(define bold? #f)
(define fg-color 'normal)
(define bg-color 'normal)
(define (fmt-control-reset)
  (set! fmt-prop #f)
  (set! fg-color 'normal)
  (set! bg-color 'normal)
  (set! bold? #f)
  (set! underline? #f))
(define (fmt-control-init)
  (fmt-control-reset)
  (fmt-control-apply))
(define (fmt-control-apply)
  (define (num x)
    (list #\; (number->string x)))
  (define (attribute)
    `(,@(if underline? (list #\; #\4 ) '())
       ,@(if bold? (list #\; #\1) '())))
  (define (fgcolor)
    (case fg-color
      ((normal) '())
      ((black)   (num 30))
      ((red)     (num 31))
      ((green)   (num 32))
      ((yellow)  (num 33))
      ((blue)    (num 34))
      ((magenta) (num 35))
      ((cyan)    (num 36))
      ((white)   (num 37))
      (else '())))

  (define (bgcolor)
    (case bg-color
      ((normal) '())
      ((black)   (num 40))
      ((red)     (num 41))
      ((green)   (num 42))
      ((yellow)  (num 43))
      ((blue)    (num 44))
      ((magenta) (num 45))
      ((cyan)    (num 46))
      ((white)   (num 47))
      (else '())))

  (apply out
         `(27 91 ;; CSI
           #\0 ;; Reset current attribute
           ,@(attribute)
           ,@(fgcolor)
           ,@(bgcolor)
           #\m)))
(define (fmt-control dat)
  (define (sym)
    (case dat
      ;; Colour operators
      ((reset)
       (fmt-control-reset))
      ((normal)
       (set! fg-color 'normal))
      ((bg-normal)
       (set! bg-color 'normal))
      ((black)
       (set! fg-color 'black))
      ((red)
       (set! fg-color 'red))
      ((green)
       (set! fg-color 'green))
      ((yellow)
       (set! fg-color 'yellow))
      ((blue)
       (set! fg-color 'blue))
      ((magenta)
       (set! fg-color 'magenta))
      ((cyan)
       (set! fg-color 'cyan))
      ((white)
       (set! fg-color 'white))
      ((bg-black)
       (set! bg-color 'black))
      ((bg-red)
       (set! bg-color 'red))
      ((bg-green)
       (set! bg-color 'green))
      ((bg-yellow)
       (set! bg-color 'yellow))
      ((bg-blue)
       (set! bg-color 'blue))
      ((bg-magenta)
       (set! bg-color 'magenta))
      ((bg-cyan)
       (set! bg-color 'cyan))
      ((bg-white)
       (set! bg-color 'white))
      ((bold)
       (set! bold? #t))
      ((no-bold)
       (set! bold? #f))
      ;; Text styles
      ((underline)
       (set! underline? #t))
      ((no-underline)
       (set! underline? #f))))
  ;; FIXME: temp.
  (sym))

;; FMT output
(define (zrepl-fmt-delete-line)
  (out 27 91 #\2 #\K "\r"))
(define (zrepl-fmt-open-line)
  (out "\r\n"))

(define (zrepl-fmt-output l)
  (define (string-out x)
    (fmt-control-apply)
    (out x))
  (define (out0 e)
    (cond
      ((or (symbol? e)
           (number? e)) 
       (fmt-control e))
      ((string? e) 
       (string-out e))
      ((pair? e) 
       (for-each out0 e)))) 
  (if (string? l)
    (string-out l)
    (out0 l)))

(define (zrepl-fmt-set-cursor x)
  (out "\r")
  (do-ec (: i x)
         (out 27 91 #\1 #\C)))

(define (zrepl-fmt-cursor-hmove d)
  (cond
    ((< d 0) ;; UP
     (out 27 91 #\1 #\A)
     (zrepl-fmt-cursor-hmove (+ d 1)))
    ((= d 0) 'ok) 
    ((> d 0) ;; Down
     (out 27 91 #\1 #\B)
     (zrepl-fmt-cursor-hmove (- d 1)))))

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
  (terminal_getheight))

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
