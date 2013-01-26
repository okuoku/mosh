(library (nmosh ffi pffi)
         (export make-pffi-ref 
                 make-callback
                 pffi-c-function
                 pffi-c-function/init
                 pffi-call/proxy)
         (import (rnrs)
                 (yuni util files)
                 (nmosh pffi interface)
                 (nmosh ffi pffi-lookup)
                 (nmosh ffi box)
                 (mosh)
                 (nmosh pffi util)
                 (nmosh pffi signatures)
                 (nmosh global-flags)
                 (mosh ffi))


(define pffi-lib (make-pffi-ref 'call-stubs))

(define pffi-call/sync
  (let ((f (get-global-flag '%nmosh-pffi-call)))
    (if f f (lambda e (assertion-violation 'pffi-call
                                           "interpreter too old")))))

(define nmosh-pffi-callback-create
  (let ((f (get-global-flag '%nmosh-pffi-callback-create)))
    (or f
        (lambda e (assertion-violation 'nmosh-pffi-callback-create
                                       "Interpreter too old")))))

(define (make-callback closure)
  (object->pointer
    (nmosh-pffi-callback-create
      (lambda e
        (let ((r (apply closure e)))
          (cond 
            ((boolean? r)
             (integer->pointer (if r 1 0)))
            ((integer? r)
             (integer->pointer r))
            ((pointer? r) r)
            ((or (string? r)
                 (bytevector? r))
             ;; FIXME: We reject them for now. These are not GC safe.
             (assertion-violation 'callback
                                  "Invalid return value"
                                  r))
            (else (integer->pointer 0))))))))

(define pffi-call-proxy #f)

(define (pffi-call/proxy rec thunk)
  (dynamic-wind
    (lambda () (set! pffi-call-proxy rec))
    thunk
    (lambda () (set! pffi-call-proxy #f))))

(define (pffi-call p ptr arg* ret)
  (if pffi-call-proxy
    (pffi-call-proxy p ptr arg* ret)
    (pffi-call/sync p ptr arg* ret)))

(define (make-pffi-call sym ptr)
  (define p (pffi-lookup pffi-lib sym))
  (lambda (arg* ret)
    (pffi-call p ptr arg* ret)))

(define (make-caller name proxy-sym ptr arg* ret)
  (define call (make-pffi-call proxy-sym ptr))
  (define arg-len (length arg*))
  (define has-ret? (not (eq? 'void ret)))
  (define (take-ret bv type)
    (case type
      ((void* char*) (ptr-box-ref bv))
      ((char)
       ;; FIXME: Little endian
       (bytevector-s8-ref bv 0))
      ((uchar)
       (bytevector-u8-ref bv 0))
      ((int long int32) ;; FIXME: long??
       (int-box-ref bv))
      ((int64)
       (bytevector-s64-native-ref bv 0))
      ((uint ulong uint32)
       (int-box-ref-unsigned bv))
      ((uint64)
       (bytevector-u64-native-ref bv 0))
      ;; IEEE Double
      ((double)
       (bytevector-ieee-double-native-ref bv 0))
      (else
        (assertion-violation name
                             "Invalid return value type"
                             type))))
  (define (fill-argument1 bv off obj type)
    (define addr (* off 8))
    (define (complain)
      (assertion-violation name
                           "Invalid argument type"
                           obj
                           off))
    (case type
      ;; Pointer
      ((void* char*)
       (cond
         ((bytevector? obj)
          (bytevector-u64-native-set! bv addr (pointer->integer
                                                (bytevector-pointer obj))))
         ((pointer? obj)
          (bytevector-u64-native-set! bv addr (pointer->integer obj)))
         ((string? obj)
          (bytevector-u64-native-set! bv addr (pointer->integer
                                                (bytevector-pointer
                                                  (string->utf8/null obj)))))
         (else (complain))))
      ;; Signed fixnums
      ((char short int long int32 int64)
       (bytevector-s64-native-set! bv addr obj))
      ;; Unsigned fixnums
      ((uchar ushort uint ulong uint32 uint64)
       (bytevector-u64-native-set! bv addr obj))
      ;; IEEE Double
      ((double)
       (bytevector-ieee-double-native-set! bv addr obj))
      ;; IEEE Float(Single)
      #|
      ((float)
       )
      |#
      (else
        (assertion-violation name
                             "Invalid type specifier"
                             type))))

  (define (fill-arguments bv off arg type*)
    (cond
      ((not (null? arg))
       (let ((obj (car arg))
             (type (car type*)))
         (fill-argument1 bv off obj type)
         (fill-arguments bv (+ 1 off) (cdr arg) (cdr type*))))))
  (lambda in
    (define argpacket (make-bytevector (* 8 arg-len)))
    (define retpacket (if has-ret? (make-bytevector 8) #f))
    ;; Sanity check
    (unless (= (length in) arg-len)
      (assertion-violation name
                           "Invalid argument"
                           in))

    ;; Construct parameter packet (64bit x N)
    (fill-arguments argpacket 0 in arg*)
    ;(write (list 'pffi-call: ptr name in))(newline)
    ;; Call
    (call (bytevector-pointer argpacket)
          (if has-ret?
            (bytevector-pointer retpacket)
            (integer->pointer 0)))
    (if has-ret?
      (take-ret retpacket ret)
      #f ;; undefined?
      )))

(define-syntax pffi-c-function
  (syntax-rules ()
    ((_ lib ret func arg ...)
     (let ((ptr (pffi-lookup lib 'func))
           (sig (string->symbol
                  (string-append "callstub_"
                                 (signature*->string '(arg ... ret))))))
       (make-caller 'func sig ptr '(arg ...) 'ret)))))

;; FIXME: Merge with pffi-c-function (without quote of funcname)
(define-syntax pffi-c-function/init
  (syntax-rules ()
    ((_ lib func)
     (let ((ptr (pffi-lookup lib func))
           (sig (string->symbol
                  (string-append "callstub_"
                                 (signature*->string '(void* void* void* 
                                                             void))))))
       (make-caller func sig ptr '(void* void* void*) 'void)))))

)
