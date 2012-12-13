(library (nmosh zrepl zprompt)
         (export
           zprompt-gadget-height
           zprompt-available?
           zprompt-start)
         (import
           (only (srfi :1)
                 take drop)
           (srfi :8)
           (srfi :42)
           (srfi :48)
           (shorten)
           (rnrs)
           (match)
           (yuni async)
           (yuni text width)
           (nmosh zrepl platform))

(define (zprompt-available?) ;; => boolean
  (zrepl-interactive?))

(define (output-length obj)
  (define (xlist l)
    (fold-left + 0 (map output-length l)))
  (cond
    ((pair? obj) (xlist obj))
    ((or (number? obj) (symbol? obj)) 0)
    ((string? obj) (string-width obj))
    ((null? obj) 0)))

(define (zprompt-gadget-height command)
  (define ret 0)
  (command 'gadget-get-height: (^[out] (set! ret out)))
  ret)

(define (zprompt-start cb/line cb) ;; => boolean
  ;; cb = ^[lineout]
  (define editline '()) ;; list-of-char
  (define cursor 0) ;; NB: It's always logical char location
  (define prompt '(green "ok > " normal))
  (define show-editline? #t)
  (define altprompt '())
  (define hint '())
  (define gadget-area '())
  (define upper-area '())
  (define lower-area '())
  (define event-route #f)

  (define (DBG . obj)
    (receive (port proc) (open-string-output-port)
      (write (list 'DBG: obj) port)
      (command (proc))))

  (define (put-with-right-aligned out left right)
    (let ((left-len (output-length left))
          (right-len (output-length right))
          (w (zrepl-output-width)))
      (cond
        ((and (not (= right-len 0)) 
              (< (+ left-len right-len) w))
         (out
           (list left
                 (string-ec (: i (- w left-len right-len 1)) #\space)
                 right)))
        (else ;; Fallback to left only
          (out left)))))

  ;; Edit command
  (define (cursor-left)
    (when (< 0 cursor)
      (set! cursor (- cursor 1))
      (redraw)))
  (define (cursor-right)
    (when (> (length editline) cursor)
      (set! cursor (+ 1 cursor))
      (redraw)))

  (define (backspace)
    (when (< 0 cursor)
      (set! editline
        (append
          (take editline (- cursor 1))
          (drop editline cursor)))
      (set! cursor (- cursor 1)))
    (redraw))
  (define (insert c)
    (set! editline
      (append
        (take editline cursor)
        (list c)
        (drop editline cursor)))
    (set! cursor (+ cursor 1))
    (redraw))

  (define (commit)
    (define ret (list->string editline))
    (put-prompt/logarea)
    (set! editline '())
    (set! cursor 0)
    (cb/line ret))

  (define (putline l)
    (zrepl-fmt-open-line)
    (zrepl-fmt-set-cursor 0)
    (zrepl-fmt-delete-line)
    (zrepl-fmt-output l))

  (define (put-prompt)
    (put-with-right-aligned
      zrepl-fmt-output
      (list prompt
            (list->string editline))
      altprompt))

  (define (put-prompt/logarea)
    (put-with-right-aligned
      put-logarea
      (list prompt
            (list->string editline))
      altprompt))

  (define (update-static-area next-gadget next-upper next-lower)
    (define (chk x fallback) (cond ((eq? #t x) fallback)
                                   (x x)
                                   (else '())))
    (define gadget (chk next-gadget gadget-area))
    (define upper0 (chk next-upper upper-area))
    (define upper (append upper0 gadget)) ;; Unified area
    (define lower (chk next-lower lower-area))

    ;; Now upper = upper + gadget
    (define current-upper (append upper-area gadget-area))
    (define current-lower lower-area)
    (define current-upper-lines (length current-upper))
    (define current-lower-lines (length current-lower))
    (define next-upper-lines (length upper)) 
    (define next-lower-lines (length lower))
    (define diff (- (+ next-upper-lines next-lower-lines)
                   (+ current-upper-lines current-lower-lines)))
    (cond
      ((<= 0 diff)
       ;; Move to top of static area
       ;; one for first line
       (zrepl-fmt-cursor-hmove (- 0 current-upper-lines 1))
       (for-each putline upper)
       (putline '()) ;; For edit area
       (for-each putline lower)
       (zrepl-fmt-cursor-hmove (- next-lower-lines)))
      (else ;; static-area got shortened
        ;; one for first line
        (zrepl-fmt-cursor-hmove (- 0 current-upper-lines 1))
        (for-each putline upper)
        (putline '()) ;; For edit area
        (for-each putline lower)
        (do-ec (: i (- diff))
               (putline '())) ;; Clear previous data
        (zrepl-fmt-cursor-hmove (- (+ (- diff) next-upper-lines)))
        (zrepl-output-vscroll (- 0 diff 1))))
    (set! gadget-area gadget)
    (set! upper-area upper0)
    (set! lower-area lower)
    ;; Draw edit area and set cursor
    (redraw))

  (define (redraw/static from-top?)
    (unless from-top?
      ;; First, move cursor to top
      (zrepl-fmt-cursor-hmove (- 0 (length upper-area)
                                 (length gadget-area))))
    (for-each putline upper-area)
    (for-each putline gadget-area)
    (putline '()) ;; For edit area
    (for-each putline lower-area)
    (zrepl-fmt-cursor-hmove (- (length lower-area)))
    (redraw))

  (define (redraw) ;; Redraw current editline
    (zrepl-fmt-set-cursor 0)
    (zrepl-fmt-delete-line)
    (put-prompt)
    (zrepl-fmt-set-cursor (+ (output-length prompt)
                             cursor)))

  (define (put-logarea obj) ;; put a line to logarea
    (zrepl-fmt-cursor-hmove (- 0 (length upper-area)
                               (length gadget-area)))
    (zrepl-fmt-set-cursor 0)
    (zrepl-fmt-delete-line)
    (zrepl-fmt-output obj)
    (redraw/static #t))

  (define (make-gadget-command command)
    (define (event . obj)
      (match obj
             ;; FIXME: Cursor position??
             (('set-gadget-area: obj)
              (update-static-area obj #t #t))

             (('gadget-get-height: cb) ;; Hidden
              (cb (- (zrepl-output-height) (+ 3 ;; For prompt + logarea
                                              (length upper-area) 
                                              (length lower-area)))))
             (else (apply command obj))))
    event)

  (define (command . obj)
    (match obj
           ;; Static area drawing
           ;; We have only this one OP to manipulate static-area.
           ;; (to avoid flickers)
           (('set-static-area: upper lower)
            ;; upper-area/lower-area will be updated in 
            ;; update-static-area
            ;;  #f means '() 
            (update-static-area #t (or upper '()) (or lower '())))

           ;; Gadget control
           (('start-gadget: gadget)
            (set! event-route gadget)
            (gadget 'activate: (make-gadget-command command)))
           (('stop-gadget: obj)
            (event-route 'leave: obj)
            (set! event-route #f)
            (update-static-area '() #t #t)
            (redraw/static #f))

           ;; Editline control
           ;; (NB: These never cause any scroll)
           (('set-prompt: . obj)
            (set! prompt obj)
            (redraw))
           (('set-right: . obj)
            (set! altprompt obj)
            (redraw))
           (('set-editline: str)
            (set! editline (string->list str))
            (set! cursor (string-length str))
            (redraw))
           (('set-hint: . obj)
            ;; Hint will be removed when user typed any character
            (set! hint obj)
            (redraw))

           ;; Quit
           ((#f)
            (do-ec (: i (length lower-area))
                   (zrepl-fmt-open-line))
            (zrepl-input-release))

           ;; Output to log area
           ((obj)
            (put-logarea obj)
            (redraw))))

  (define (char-event c ctrl? alt? super?)
    (cond
      ((not (or ctrl? alt? super?))
       (insert c))
      (ctrl?
        (case c
          ((#\a #\A)
           (set! cursor 0)
           (redraw))
          ((#\c #\C) 
           (put-prompt/logarea)
           (set! editline '())
           (set! cursor 0)
           (redraw))
          ((#\j #\J) (commit))))))

  (define (symbol-event sym ctrl? alt? super?)
    (case sym
      ((up) (cb/line 'prev?))
      ((down) (cb/line 'next?))
      ((left) (cursor-left))
      ((right) (cursor-right))
      ((backspace) (backspace)))
    'ok)
  (define (zrepl-event . obj)
    ;(DBG `(Event: ,obj))
    (match obj
           ((x ctrl? alt? super?)
            (cond
              (event-route
                (event-route 'key: x ctrl? alt? super?))
              ((char? x)
               (char-event x ctrl? alt? super?))
              ((symbol? x)
               (symbol-event x ctrl? alt? super?))))
           ((#f obj)
            (assertion-violation 'zprompt
                                 "something wrong"
                                 obj))))
  (zrepl-input-acquire)
  (zrepl-input-subscribe zrepl-event)
  (cb command)
  (redraw)
  #t)

)
