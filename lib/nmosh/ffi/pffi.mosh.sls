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

(define (pffi-call p ptr argc . argv)
  (if pffi-call-proxy
    (apply pffi-call-proxy p ptr argc argv)
    (apply pffi-call/sync p ptr argc argv)))

(define (make-pffi-call sym ptr)
  (define p (pffi-lookup pffi-lib sym))
  (lambda arg*
    (apply pffi-call p ptr (length arg*) arg*)))

(define (make-caller name proxy-sym ptr arg* ret)
  (define call (if ptr
                 (make-pffi-call proxy-sym ptr)
                 (lambda e
                   (assertion-violation 'pffi
                                        "PFFI function not found"
                                        name))))
  (define arg-len (length arg*))
  (define (take-ret ptr)
    (define (bv)
      (define out (make-ptr-array 1))
      (ptr-array-set! out 0 ptr)
      out)
    (case ret
      ((void) 
       (values))
      ((void* char*) ptr)
      ((char)
       ;; FIXME: Little endian
       (bytevector-s8-ref (bv) 0))
      ((int long int32) ;; FIXME: long??
       (ptr-box-ref-signed (bv)))
      ((int64)
       (bytevector-s64-native-ref (bv) 0))
      ((uchar uint64 uint ulong uint32)
       (pointer->integer ptr))
      ;; IEEE Double
      ((double)
       (bytevector-ieee-double-native-ref (bv) 0))
      (else
        (assertion-violation name
                             "Invalid return value type"
                             ret))))
  (define (conv-argument type obj)
    (define (complain)
      (assertion-violation name
                           "Invalid argument type"
                           obj))
    ;; FIXME: Add type check here.
    (case type
      ;; Pointer
      ((void* char*)
       (cond
         ((bytevector? obj) obj)
         ((pointer? obj) obj)
         ((string? obj)
          (string->utf8/null obj))
         (else (complain))))
      ;; Signed fixnums
      ((char short int long int32 int64) obj)
      ;; Unsigned fixnums
      ((uchar ushort uint ulong uint32 uint64) obj)
      ;; IEEE Double
      ((double) obj)
      ;; IEEE Float(Single)
      #|
      ((float)
       )
      |#
      (else
        (assertion-violation name
                             "Invalid type specifier"
                             type))))
  (lambda in
    ;; Sanity check
    (unless (= (length in) arg-len)
      (assertion-violation name
                           "Invalid argument length"
                           in))
    (let* ((args (map conv-argument arg* in))
           (ret (apply call args)))
      ;(write (list 'pffi-call: ptr name in))(newline)
      (cond
        ((pointer? ret)
         (take-ret ret))
        (else
          (assertion-violation name
                               "Invalid result"
                               ret))))))

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
