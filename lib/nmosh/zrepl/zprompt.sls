(library (nmosh zrepl zprompt)
         (export
           zprompt-available?
           zprompt-start)
         (import
           (only (srfi :1)
                 take drop)
           (srfi :8)
           (srfi :42)
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
    ((string? obj) (string-width obj))))

(define (zprompt-start cb/line cb) ;; => boolean
  ;; cb = ^[lineout]
  (define editline '()) ;; list-of-char
  (define cursor 0) ;; NB: It's always logical char location
  (define prompt '(green "ok > " normal))
  (define show-editline? #t)
  (define altprompt '())
  (define hint '())
  (define upper-area '())
  (define lower-area '())

  (define (DBG . obj)
    (receive (port proc) (open-string-output-port)
      (write (list 'DBG: obj) port)
      (command (proc))))

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
    (set! editline '())
    (set! cursor 0)
    (put-logarea (append prompt (list ret)))
    (cb/line ret))

  (define (putline l)
    (zrepl-fmt-open-line)
    (zrepl-fmt-set-cursor 0)
    (zrepl-fmt-delete-line)
    (zrepl-fmt-output l))

  (define (update-static-area upper lower)
    (define current-upper upper-area)
    (define current-lower lower-area)
    (define current-upper-lines (length upper-area))
    (define current-lower-lines (length lower-area))
    (define next-upper-lines (length upper))
    (define next-lower-lines (length lower))
    (define diff (- (+ current-upper-lines current-lower-lines)
                    (+ next-upper-lines next-lower-lines)))
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
        (do-ec (: i diff)
               (putline '())) ;; Clear previous data
        (zrepl-fmt-cursor-hmove (- (+ diff next-lower-lines)))))
    (set! upper-area upper)
    (set! lower-area lower)
    ;; Draw edit area and set cursor
    (redraw))

  (define (redraw/static from-top?)
    (unless from-top?
      ;; First, move cursor to top
      (zrepl-fmt-cursor-hmove (- (length upper-area))))
    (for-each putline upper-area)
    (putline '()) ;; For edit area
    (for-each putline lower-area)
    (zrepl-fmt-cursor-hmove (- (length lower-area)))
    (redraw))

  (define (redraw) ;; Redraw current editline
    (zrepl-fmt-set-cursor 0)
    (zrepl-fmt-delete-line)
    (zrepl-fmt-output (list prompt
                            (list->string editline)))
    (zrepl-fmt-set-cursor (+ (output-length prompt)
                             cursor)))

  (define (put-logarea obj) ;; put a line to logarea
    (zrepl-fmt-cursor-hmove (- (length upper-area)))
    (zrepl-fmt-set-cursor 0)
    (zrepl-fmt-delete-line)
    (zrepl-fmt-output obj)
    (redraw/static #t))

  (define (command . obj)
    (match obj
           ;; Static area drawing
           ;; We have only this one OP to manipulate static-area.
           ;; (to avoid flickers)
           (('set-static-area: upper lower)
            ;; upper-area/lower-area will be updated in 
            ;; update-static-area
            ;;  #f means '() 
            (update-static-area (or upper '()) (or lower '())))

           ;; Editline control
           ;; (NB: These never cause any scroll)
           (('set-prompt: . obj)
            (set! prompt obj)
            (redraw))
           (('set-altprompt: . obj)
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
           (put-logarea (append prompt (list (list->string editline))))
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
