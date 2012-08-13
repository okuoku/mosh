(library (nmosh graphics ext cairo drawops0)
         (export pack-drawop)
         (import (rnrs)
                 (shorten)
                 (nmosh ffi box)
                 (nmosh pffi interface)
                 (nmosh stubs mosh-cairo)
                 (srfi :8))
;;

(define (make-matrix-array n)
  (make-bytevector (* 6 8 n)))

(define-syntax extract-drawop
  (syntax-rules ()
    ((_ h (name v ...))
     (hashtable-set! h 'name '(v ...)))))

(define-syntax define-drawop
  (syntax-rules ()
    ((_ name op ...)
     (define name (let ((h (make-eq-hashtable)))
                    (extract-drawop h op)
                    ...
                    h)))))

;;
(define-drawop
  drawops
  (move-to 1 V V) 
  (curve-to 2 V V V V V V) 
  (line-to 3 V V) 
  (close 4) 
  (select 10 O) 
  (source 11 O) 
  (winding-even/odd 12) 
  (winding-nonzero 13) 
  (stroke 14) 
  (stroke/preserved 15) 
  (fill 16) 
  (fill/preserved 17) 
  (width 18 V) 
  (set-transform 19 M) 
  (load-transform 20 M) 
  (paint 21) 
  (paint/alpha 22 V) 
  (set-operator 23 M)
  (set-transform-source 101 O M) 
  (matrix-identity 201 M)
  (matrix-load 202 M V V V V V V) 
  (matrix-translate 203 M V V) 
  (matrix-scale 204 M V V) 
  (matrix-rotate 205 M V) 
  (matrix-invert 206 M) 
  (matrix-mul 207 M M M) 
  (matrix-copy 208 M M)) 

(define (put-base128 p v)
  (if (>= v 128)
    (let ((m (mod v 128))
          (d (div v 128)))
      (put-u8 p (+ m 128))
      (put-base128 p d))
    (put-u8 p v)))

(define (v->bv v)
  (define bv (make-bytevector 8))
  (bytevector-ieee-double-native-set! bv 0 v)
  bv)

(define (obj->bv o)
  (define bv (make-ptr-box))
  (ptr-box-set! bv o)
  bv)

(define (pack-drawop l) ;; => op v obj objcount
  ;; M := id
  ;; O := object
  ;; V := double

  (define obj-count 0)
  (define obj-hash (make-eq-hashtable))
  (define (do-pack p-op p-vtx p-obj)
    (define (obj x)
      (or (hashtable-ref obj-hash (pointer->integer x) #f)
          (begin
            (put-bytevector p-obj (obj->bv x))
            (hashtable-set! obj-hash (pointer->integer x) obj-count)
            (set! obj-count (+ obj-count 1))
            (- obj-count 1))))
    (define (procarg sym arg)
      (case sym
        ((V)
         (put-bytevector p-vtx (v->bv arg)))
        ((M)
         (put-base128 p-op arg))
        ((O)
         (put-base128 p-op (obj arg)))
        (else
          (assertion-violation 'pack-drawop
                               "Invalid argtype"
                               sym arg))))
    (define vcount 0)
    (define (itr l)
      (when (pair? l)
          (let ((name (caar l))
                (args (cdar l))
                (next (cdr l)))
            (if (pair? name)
              ;; Depth first
              (begin
                (itr (list name))
                (itr args))
              ;; Else
              (let ((code (hashtable-ref drawops name #f)))
                (unless code
                  (assertion-violation 'pack-drawop
                                       "Invalid opcode"
                                       name
                                       args))
                (unless (= (length args) (length (cdr code)))
                  (assertion-violation 'pack-drawop
                                       "Invalid argument"
                                       name
                                       args))
                (put-u8 p-op (car code))
                (for-each procarg (cdr code) args))) 
            (itr next))))
    (itr l)
    vcount)

  (receive (p-op proc-op) (open-bytevector-output-port)
    (receive (p-vtx proc-vtx) (open-bytevector-output-port)
      (receive (p-obj proc-obj) (open-bytevector-output-port)
        (do-pack p-op p-vtx p-obj) 
        (values (proc-op)
                (proc-vtx) 
                (proc-obj) obj-count)))))

)
