(library (nmosh zrepl zprompt)
         (export
           zprompt-available?
           zprompt-start
           )
         (import
           (only (srfi :1)
                 take drop)
           (srfi :8)
           (rnrs)
           (match)
           (yuni async)
           (nmosh zrepl platform posix))

(define (zprompt-available?) ;; => boolean
  (zrepl-interactive?))

(define (zprompt-start cb/line cb) ;; => boolean
  (define editline '()) ;; list-of-char
  (define cursor 0)
  (define prompt "ok > ")

  (define (DBG . obj)
    (receive (port proc) (open-string-output-port)
      (write (list 'DBG: obj) port)
      (lineout (proc))))

  ;; Edit command
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
    (cb/line (list->string editline))
    (set! editline '())
    (set! cursor 0)
    (zrepl-fmt-open-line)
    (redraw))


  ;; cb = ^[lineout]
  (define (redraw)
    (zrepl-fmt-set-cursor 0)
    (zrepl-fmt-delete-line)
    (zrepl-fmt-output (list prompt
                            (list->string editline)))
    (zrepl-fmt-set-cursor (+ (string-length prompt)
                             cursor)) )
  (define (lineout obj)
    (cond
      (obj
        (zrepl-fmt-delete-line)
        (zrepl-fmt-output obj)
        (zrepl-fmt-open-line)
        (redraw))
      (else ;; #f for terminate
        (zrepl-input-release))))

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
      ((backspace) (backspace)))
    'ok)
  (define (zrepl-event . obj)
    (DBG `(Event: ,obj))
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
  (cb lineout)
  #t)

)
