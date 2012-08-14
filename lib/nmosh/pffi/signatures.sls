;; NB: Should be R6RS portable
(library (nmosh pffi signatures)
         (export 
           signature->c-arg*+ret
           signature*->string)
         (import (rnrs)
                 (shorten)
                 (match))
(define type-map
  '(
    ;; FIXME: Mosh local type
    (fn p "void*")

    ;; Basic C types (took from dyncall et al)
    (void v "void")
    (void* p "void*")

    ;; Integer C types
    (char c "signed char")
    (uchar C "unsigned char")
    (short s "short")
    (ushort S "unsigned short")
    (int i "int")
    (uint I "unsigned int")
    (long j "long")
    (ulong J "unsigned long")
    ;(longlong l #t)
    ;(unsigned-longlong L #t)

    ;; Floating point C types
    (double d "double")
    (float f "float")

    ;; Stdint C types (private extension)
    (int32 y "int32_t")
    (uint32 Y "uint32_t")
    (int64 z "int64_t")
    (uint64 Z "uint64_t")))

(define rtype-map 
  (map (^e (match e ((x y z) (list y x z))))
       type-map))

;; signature := <type>* '_' <return-type>
(define (signature*->string l)
  (define (to-char sym)
    (car (string->list (symbol->string sym))))
  (define (calc type)
    (define p (assq type type-map))
    (unless p 
      (display (list 'unknown-type: type "Assuming as pointer"))(newline))
    ;; FIXME: Use pointer to denote unknown types
    (or (and p (match p ((sym out ctype) out))) 
        'p))
  (let* ((e (reverse (map calc l)))
         (last (list (car e)))
         (rest (reverse (cdr e))))
    (list->string (map to-char (append rest '(_) last)))))

(define (signature/sym->c-type c)
  (define type (string->symbol (list->string (list c))))
  (define p (assq type rtype-map))
  (and p
       (match p ((sym out ctype) ctype))))

(define (signature->c-arg*+ret str) ;; => type* ret
  (define s* (string->list str))
  (define (itr rarg* rest)
    (match rest
           ((#\_ ret)
            (values (reverse rarg*)
                    (signature/sym->c-type ret)))
           ((top . next)
            (itr (cons (signature/sym->c-type top)
                       rarg*)
                 next))))
  (itr '() s*))

)
