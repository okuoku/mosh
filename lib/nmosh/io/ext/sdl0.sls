(library (nmosh io ext sdl0)
         (export msdl-init
                 msdl-event-poll
                 msdl-event-read-pointing
                 msdl-event-size
                 msdl-window-destroy
                 msdl-window-create
                 msdl-window-show)
         (import
           (rnrs)
           (yuni core)
           (shorten)
           (match)
           (srfi :8)
           (nmosh ffi box)
           (nmosh pffi util)
           (nmosh pffi interface)
           (nmosh io core)
           (nmosh io master-queue)
           (nmosh aio platform)
           (nmosh stubs mosh-sdl))

;;


(define (msdl-event-poll bv)
  (msdl_event_poll bv))
(define (msdl-event-read-pointing bv) ;; => class window-id id action x y dx dy
  (let ((class (make-int-box))
        (window-id (make-int-box))
        (id (make-ptr-box))
        (action (make-int-box))
        (x (make-int-box))
        (y (make-int-box))
        (dx (make-int-box))
        (dy (make-int-box)))
    (let ((r (msdl_event_read_pointing bv class window-id id action x y 
                                        dx dy)))
      (if (= r 0) ;; fail?
        (values #f #f #f #f #f #f #f #f)
        (values (int-box-ref class)
                (int-box-ref window-id)
                (ptr-box-ref-unsigned id)
                (int-box-ref action)
                (int-box-ref x)
                (int-box-ref y)
                (int-box-ref dx)
                (int-box-ref dy))))))

(define (msdl-event-read-type bv)
  (msdl_event_read_type bv))
(define (msdl-event-size)
  (msdl_event_size))
(define (msdl-window-surface-get win)
  (msdl_window_surface_get))
(define (msdl-window-id win)
  (msdl_window_id win))
(define (msdl-window-update win from to x y w h dest-x dest-y)
  (msdl_window_update win from to x y w h dest-x dest-y))
(define (msdl-window-destroy win)
  (msdl_window_destroy win))
(define (msdl-window-show win)
  (msdl_window_show win))
(define (msdl-window-create title x y w h)
  (define (q x) (or x -1))
  (msdl_window_create (string->utf8/null title)
                      (q x)
                      (q y)
                      w h))
(define (msdl-init) (msdl_init))

;; async version
#| 
(define (msdl-subscribe cb enqueue-return)
  (define callbacks '())
  (define (dequeue-cb)
    (cond
      ((pair? callbacks)
       (let ((a (car callbacks))
             (d (cdr callbacks)))
         (set! callbacks d)
         a))
      (else #f)))
  (define (enqueue-cb cb)
    (set! callbacks (append callbacks (list cb))))
  (define (enqueue evt . param)
    (case evt
      ((update:)
       (match param
              ((win from to x y w h x1 y1 cb)
               (msdl_enqueue_window_create
                 win from to x y w h x1 y1)
               (enqueue-cb cb))))
      ((window-create:)
       (match param
              ((name x y w h cb)
               (define auto-position? (not x))
               (display (list 'window-create x))(newline)
               (if auto-position?
                 (msdl_enqueue_window_create
                   (string->utf8 name)
                   -1 -1 w h 0)
                 (msdl_enqueue_window_create
                   (string->utf8 name)
                   x y w h 0))
               (enqueue-cb cb))))))
  (define (callback out0 out1)
    (write (list 'sdl-callback out0 out1))(newline)
    (case out0
      ((0)
       (case out1
         ((1)
          (enqueue-return enqueue))
         ((2)
          ((dequeue-cb) #t))))
      ((1)
       ((dequeue-cb) (integer->pointer out1)))
      (else
        (cb 'unknown-event out0)
        (msdl_event_dispose (integer->pointer out0)))))
  (queue-invoke-ffithread nmosh-io-master-queue
                          (msdl_getcoreloop_func)
                          0
                          0
                          callback))
|#
)
