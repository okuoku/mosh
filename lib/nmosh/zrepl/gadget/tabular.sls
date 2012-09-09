(library (nmosh zrepl gadget tabular)
         (export gadget-tabular)
         (import (rnrs)
                 (shorten)
                 (match)
                 (yuni core)
                 (srfi :42)
                 (yuni text tabular)
                 (nmosh zrepl zprompt)
                 (nmosh zrepl platform))

;;

(define (list-range l off len)
  (list-ec (: i len)
           (list-ref l (+ off i))))

(define (set-cursor l at)
  (if (and (<= 0 at) (< at (length l)))
    (let ((cur (list-ref l at))
          (v (list->vector l)))
      ;; Braindamaged
      (vector-set! v at (list 'reverse cur 'no-reverse))
      (vector->list v))
    ;; Unmodified if out of range
    l))

(define* (gadget-tabular #((title: #f)
                           (show-header: #f))
                         dat callback)
  ;; dat := (#(#f NAME1 NAME2 ...)
  ;;          (datum datum datum ...)
  ;;          ...)
  ;;
  (define output #f)
  (define header #f)
  (define content #f)
  (define offset 0)
  (define cursor 0)
  (define height 0)
  (define show-header show-header:)
  (define title title:)
  (define current-width 0)
  (define current-tbl #f)
  (define (draw draw-to-logarea?)
    (define width (zrepl-output-width))
    (define out-height (zprompt-gadget-height output))
    (define out '())
    (define (gen-header)
      (list 'bold 'blue 'reverse header 'no-reverse 'normal 'no-bold))

    ;; Recalc tbl when needed
    (when (not (and current-tbl
                    (eq? current-width width)))
      (set! current-tbl (format-tabular dat 'width-limit: width)))

    ;; Construct data area
    (let* ((content-height (- (length current-tbl) 1))
           (max-dat-height (- out-height 1)) ;; 1 for header
           (dat-scroll-start (floor (/ max-dat-height 2)))
           (max-offset (- content-height max-dat-height)))
      (cond
        ((> max-dat-height content-height)
         ;; Output verbatim
         (set! offset 0)
         (set! out (cdr current-tbl)))
        (else
          ;; cut
          (cond
            (cursor
              ;; Update offset with current height
              (cond
                ((< cursor dat-scroll-start)
                 (set! offset 0))
                (else
                  (set! offset (min max-offset
                                    (- cursor dat-scroll-start)))))
              (set! out (list-range (cdr current-tbl) offset max-dat-height)))
            (else
              ;; Assume offset 0
              (set! out (list-range (cdr current-tbl) 0 max-dat-height)))))))

    ;; Show cursor (when enabled)
    ;; Assume out == data area here. (Add our header later.)
    (set! out (set-cursor out (- cursor offset)))

    ;; Add header (when enabled)
    (when show-header
      ;; FIXME: How do we generate header?
      (set! header (car current-tbl))
      (set! out (cons (gen-header) out)))

    (cond
      (draw-to-logarea?
        (for-each output out))
      (else
        (output 'set-gadget-area: out))))

  (define (redraw) (draw #f))

  (define (leave out-current?)
    (when out-current?
      (draw #t)))


  ;; Output
  (define (decide obj)
    (callback obj))

  ;; Event actions
  (define (top)
    (set! offset 0)
    (set! cursor (and cursor 0))
    (redraw))
  (define (bottom)
    ;; Length - Header - 1
    (set! cursor (- (length current-tbl) 2))
    (redraw))

  (define (cancel)
    (decide #f))
  (define (move-down offs)
    (set! cursor (min (- (length current-tbl) 2)
                      (+ cursor offs)))
    (redraw))

  (define (move-up offs)
    (set! cursor (max 0 (- cursor offs)))
    (redraw))
  (define (down) (move-down 1))
  (define (up) (move-up 1))

  (define (page-down) (move-down 5))
  (define (page-up) (move-up 5))
  (define (commit)
    (decide
      (and cursor
           (list-ref dat (+ cursor 1)))))

  ;; Input event handlers
  (define (char-event c)
    (case c
      ((#\g) (top))
      ((#\G) (bottom))
      ((#\Q #\q) (cancel))
      ((#\J #\j) (down))
      ((#\K #\k) (up))
      ((#\space) (page-down))
      ((#\B #\b) (page-up))))
  (define (char-event/ctrl c)
    (case c
      ((#\j #\J #\M #\m) (commit))
      ((#\f #\f) (page-down))
      ((#\b #\B) (page-up))))
  (define (sym-event sym)
    (case sym
      ((up) (up))
      ((down) (down))
      ((left) (page-up))
      ((right) (page-down))))
  
  (define (key . obj)
    (match obj
           ((x ctrl? alt? super?)
            (cond
              ((and (char? x) (not ctrl?))
               (char-event x))
              ((and (char? x))
               (char-event/ctrl x))
              ((symbol? x)
               (sym-event x))))
           (else 'ignore)))

  (define (event . obj)
    (match obj
           (('activate: out)
            (set! output out)
            (set! cursor 0)
            (redraw))
           (('show-header: opt)
            (set! show-header opt)
            (redraw))
           (('leave: out-current?)
            (leave out-current?))
           (('key: . input)
            (apply key input))))
  event)

)
