(library (nmosh zrepl zprompt)
         (export
           zprompt-available?
           zprompt-start)
         (import
           (only (srfi :1)
                 take drop)
           (srfi :8)
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
  (define altprompt '())
  (define hint '())

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
    (zrepl-fmt-open-line)
    (redraw)
    (cb/line ret))

  (define (redraw)
    (zrepl-fmt-set-cursor 0)
    (zrepl-fmt-delete-line)
    (zrepl-fmt-output (list prompt
                            (list->string editline)))
    (zrepl-fmt-set-cursor (+ (output-length prompt)
                             cursor)))
  (define (command . obj)
    (match obj
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
            (set! hint obj))
           ;; Quit
           ((#f)
            (zrepl-input-release))
           ;; Output 
           ((obj)
            (zrepl-fmt-delete-line)
            (zrepl-fmt-output obj)
            (zrepl-fmt-open-line)
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
           (zrepl-fmt-open-line)
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
