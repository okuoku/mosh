(library (nmosh net msgpack rpc)
         (export 
           make-msgpack-rpc-server-socket
           make-msgpack-rpc-client-socket
           rpc-server
           else =>)
         (import (rnrs)
                 (shorten)
                 (match)
                 (srfi :8)
                 (nmosh net msgpack))
;;

(define nothing-to-do (^ _ 'nothing-to-do))
;;; msgpack rpc format
;; request = [0, msgid, method, params]
;; response = [1, msgid, error, result] ;;; error = Nil if OK
;; notification = [2, method, params]
(define (match-invoke dmap method param-len params)
  (define (next obj) (match-invoke obj method param-len params))
  (define (call p) (apply p params))
  (match dmap
         ((((name . count) . proc) . n)
          (cond
            ((bytevector=? name method)
             (if count
               (if (= count param-len)
                 (call proc)
                 (next n))
               (call proc)))
            (else (next n))))
         (else
           (assertion-violation 'match-invoke
                                "Invalid method"
                                (utf8->string method)))))

(define-syntax %gen-dispatch-map1
  ;; Generate argmap
  ;; (NAME count LAMBDA)
  ;; (NAME #f LAMBDA) ;; vararg
  (syntax-rules ()
    ((_ ((name . arg) body ...))
     (let ((vararg? (not (list? '(name . arg)))))
       (cons
         (cons (string->utf8 (symbol->string 'name))
               (if vararg? #f (length  'arg)))
         (lambda arg body ...))))))
(define-syntax %gen-dispatch
  (syntax-rules ()
    ((_ session clause ...)
     (let ((dmap (list (%gen-dispatch-map1 clause) ...))) 
       ;; NB: We pass "session" here.
       (^[session method params result-cb]
         (define param-len (if (pair? params) (length params) 1))
         ;; (write (list 'dispatch: dmap param-len method))(newline)
         (call-with-values (^() (match-invoke dmap method param-len params))
                           (^ obj (result-cb obj))))))))

(define-syntax rpc-server
  (syntax-rules (else =>)
    ((_ session clause ...  (else => proc))
     (^ obj
        (define dispatch (%gen-dispatch session clause ...))
        (match obj
               ((#t obj)
                (proc #t obj))
               ((#f obj)
                (proc #f obj))
               ((session method params result-cb)
                (dispatch session method params result-cb))
               ((session #f obj) ;; Session out
                (proc #f session))
               (else
                 (assertion-violation 'rpc-server
                                      "Unexpected message format"
                                      obj)))))
    ((_ clause ...)
     (rpc-server clause ... (else => nothing-to-do)))))

(define (make-msgpack-rpc-server-socket host port session-cb note-cb result)
  ;; session-cb = ^[obj] => session
  ;;    (session obj) => session ;; New session
  ;;    (session #f) ;; Error, close
  ;; note-cb = ^[session? ....]
  ;;    (note session method params result-cb) ;; Call
  ;;    (note session #t method params) ;; Nortification
  ;;    (note session #f obj) ;; Socket error, close
  ;;    (note #t obj) ;; Ignored object
  ;;    (note #f obj) ;; Server socket error, close
  ;; result = ^[ok? obj]

  (define server-started? #f)
  (define (new-session serv fd inetname)
    (define session-id #f)
    (define out #f)
    (define close #f)
    (start-msgpack-talker
      fd
      (^[obj]
        (unless session-id
          (assertion-violation 'msgpack-rpc-session
                               "invalid session"
                               obj))
        (match obj
               ((0 id method params)
                (note-cb session-id method params
                         (^[obj]
                           ; (write (list 'out: id obj))(newline)
                           (out (list 1 id '() obj) nothing-to-do))))
               ((2 method params)
                (note-cb session-id #t method params))
               (else
                 (note-cb #t obj))))
      (^[writer close-proc]
        (define session-obj (session-cb session-id))
        (set! close close-proc)
        (set! out writer)
        (set! session-id session-obj))
      (^[fd]
        (when session-id
          (note-cb session-id #f session-id)))))

  (define (error-callback fd)
    (if server-started?
      (note-cb #f #f)
      (result #f #f)))
  (define (send-result ok? obj)
    (when ok?
      (set! server-started? #t))
    (result ok? obj))
  (make-msgpack-server-socket 
    host port
    new-session
    error-callback
    send-result))

(define (make-msgpack-rpc-client-socket host port note-cb call-cb)
  ;; note-cb = ^[ok? . obj]
  ;;    (note #t method params) ;; Notification
  ;;    (note #f method result orig-cb) ;; Method error
  ;;    (note #t obj) ;; Ignored object
  ;;    (note #f obj) ;; Socket error, close
  ;; call-cb = ^[caller/#f]
  ;;    (call #f) ;; Shutdown
  ;;    (call #f cb) ;; Shutdown with complete callback
  ;;    (call #t method params) ;; Send notification
  ;;    (call method params cb) ;; Call
  (define connected? #f)
  (define out #f)
  (define close #f)
  (define id 1)
  (define (make-request-ht) (make-eq-hashtable))
  (define request-ht (make-request-ht))
  (define (lookup id) ;; => method obj
    (let ((r (hashtable-ref request-ht id #f)))
      (if r
        (values (car r) (cdr r))
        (values #f #f))))
  (define (register method id obj)
    (hashtable-set! request-ht id (cons method obj)))
  (define (unregister id)
    (hashtable-delete! request-ht id))
  (define (receiver obj)
    ; (write (list 'recv: obj))(newline)
    (match obj
           ((1 msgid err result)
            (receive (method cb) (lookup msgid)
              ; (write (list 'lookup: cb msgid))(newline)
              (if cb
                (if (null? err)
                  (cb result)
                  (note-cb #f method result cb))
                (note-cb #t obj))
              (unregister msgid)))
           ((2 method params)
            (note-cb #t method params))
           (else
             (note-cb #t obj))))
  (define (error-callback obj)
    (if connected?
      (note-cb #f #f)
      (call-cb #f)))
  (define (request . obj)
    (match obj
           ((#f) ;; Shutdown
            (close #f nothing-to-do))
           ((#f cb) ;; Shutdown w/ callback
            ;; FIXME: Implement completion
            (close #t cb))
           ((#t method obj) ;; Send notification
            (out (list 2 method obj) nothing-to-do))
           ((met params cb) ;; call
            (define method (cond
                             ((symbol? met) (symbol->string met))
                             (else met)))
            (set! id (+ 1 id))
            (register method id cb)
            (out (list 0 id method params) nothing-to-do))
           (else
             (assertion-violation 'msgpack-rpc-request
                                  "Invalid argument"
                                  obj))))
  (define (write-cb write-proc close-proc)
    (set! connected? #t)
    (set! close close-proc)
    (set! out write-proc)
    (call-cb request))

  (make-msgpack-client-socket
    host port
    receiver
    write-cb
    error-callback))

)
