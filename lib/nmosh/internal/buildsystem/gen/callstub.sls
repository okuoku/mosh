(library (nmosh internal buildsystem gen callstub)
         (export
           gen-callstub
           )
         (import (rnrs) (yuni core) (srfi :8)
                 (nmosh pffi signatures)
                 (shorten)
                 (srfi :26)
                 (srfi :48) )

(define (gen-callstub p funcname rettype argtype* callname) ;; => c-symname
  (define directcall? (and callname #t))
  (define true-callname (or callname 'func))
  (define c-symname (format "callstub_~a" funcname))
  (define typename (if directcall?
                     "void*"
                     (format "func_~a_t" funcname)))
  (define (gen-arg with-paramname?)
    (define count 0)
    (define (proc e)
      (or (and with-paramname?
               (begin
                 (let ((out (format "*(~a *)(char *)&args[~a]" e count)))
                   (set! count (+ 1 count))
                   out)))
          e))
    (if (null? argtype*)
      ""
      (receive (port out) (open-string-output-port)
        ;; First, without comma
        (format port "~a" (proc (car argtype*)))
        ;; Second, ..., with comma
        (for-each (^e (format port ", ~a" (proc e)))
                  (cdr argtype*))
        (out))))
  (unless directcall?
    ;; Define callback type 
    (format p "typedef ~a (*~a)(~a);\n"
            rettype typename (gen-arg #f)))
  (format p "static void\n")
  (format p "~a(~a ~a, uint64_t* args, void* ret){\n"
          c-symname typename (if directcall? 'bogus 'func))

  ;; FIXME: Consume unused variables here....

  ;; Generate return-store
  (case rettype
    ((void)
     (format p "    ~a(~a);\n}\n\n" true-callname (gen-arg #t)))
    ((float)
     (format p "    *(float*)ret = ~a(~a);\n}\n\n"
             true-callname (gen-arg #t)) )
    ((double)
     (format p "    *(double*)ret = ~a(~a);\n}\n\n"
             true-callname (gen-arg #t)) )
    (else
      (format p "    *(uintptr_t*)ret = ~a(~a);\n}\n\n"
              true-callname (gen-arg #t)) ) )
  c-symname)

)
