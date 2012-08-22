(library (yuni binary format elf0)
         (export elf64le-lookup-sectionheader
                 elf64le-symbol-list)
         (import (rnrs)
                 (srfi :42)
                 (yuni binary macro packet0)
                 (yuni core))

(define-packet0*
  elf64le-sectionheader pack-elf64le-sectionheader unpack-elf64le-sectionheader
  (name "unsigned" 4 little)
  (type "unsigned" 4 little)
  (flags "unsigned" 8 little)
  (addr "unsigned" 8 little)
  (offset "unsigned" 8 little)
  (size "unsigned" 8 little)
  (link "unsigned" 4 little)
  (info "unsigned" 4 little)
  (addralign "unsigned" 8 little)
  (entsize "unsigned" 8 little)
  )

(define-packet0*
  elf64le-fileheader pack-elf64le-fileheader unpack-elf64le-fileheader
  (ident "blob" 16)
  (type "unsigned" 2 little)
  (machine "unsigned" 2 little)
  (version "unsigned" 4 little)
  (entry "unsigned" 8 little)
  (phoff "unsigned" 8 little)
  (shoff "unsigned" 8 little)
  (flags "unsigned" 4 little)
  (ehsize "unsigned" 2 little)
  (phentsize "unsigned" 2 little)
  (phnum "unsigned" 2 little)
  (shentsize "unsigned" 2 little)
  (shnum "unsigned" 2 little)
  (shstrndx "unsigned" 2 little))

(define-packet0*
  elf-ident pack-elf-ident unpack-elf-ident
  (magic "blob" 4) ;; 0x7f ELF
  (class "unsigned" 1)
  (data "unsigned" 1)
  (abi "unsigned" 1)
  (abiversion "unsigned" 1))

(define-packet0*
  elf64le-sym pack-elf64le-sym unpack-elf64le-sym
  (name "unsigned" 4 little)
  (info "unsigned" 1)
  (other "unsigned" 1)
  (shndx "unsigned" 2 little)
  (value "unsigned" 8 little)
  (size "unsigned" 8 little))

(define (elf64le-symbol-list bv)
  (define hdr (elf64le-lookup-sectionheader bv ".symtab"))
  (define str (elf64le-lookup-sectionheader bv ".strtab"))
  (define str-off (and str (~ str 'offset)))
  (define (decinfo x)
    (case (bitwise-and x #xf)
      ((0) 'NOTYPE)
      ((1) 'OBJECT)
      ((2) 'FUNC)
      ((3) 'SECTION)
      ((4) 'FILE)
      ((5) 'COMMON)
      ((6) 'TLS)
      ((7) 'NUM) ;; ???
      ))
  (define (decother x)
    (case x
      ((0) 'DEFAULT)
      ((1) 'INTERNAL)
      ((2) 'HIDDEN)
      ((3) 'PROTECTED)
      ((4) 'EXPORTED)
      ((5) 'SINGLETON)
      ((6) 'ELIMINATE)
      (else x)))
  (define (ret x)
    (list
      (lookup-strtab bv (+ str-off (~ x 'name)))
      (decinfo (~ x 'info))
      (decother (~ x 'other))
      (~ x 'value)
      (~ x ' size)))
  (define (pickup off size)
    (define lim (+ off size))
    (define (itr cur off)
      (if (< off lim)
        (itr (cons (unpack-elf64le-sym bv off) cur)
             (+ off (+ 4 1 1 2 8 8)))
        (map ret (reverse cur))))
    (itr '()  off))
  (and hdr
       str
       (pickup (~ hdr 'offset) (~ hdr 'size))))

(define (lookup-strtab bv off) ;; => string
  ;; lookup asciiz => string
  (define (itr o cur)
    (define v (bytevector-u8-ref bv o))
    (if (= v 0)
      (utf8->string (u8-list->bytevector (reverse cur)))
      (itr (+ o 1) (cons v cur))))
  (itr off '()))

(define (elf64le-lookup-sectionheader bv name) ;; => elf64le-sectionheader
  (define fileheader (unpack-elf64le-fileheader bv 0))
  (define (locate str-off l)
    (and (pair? l)
         (or (and
               (string=? (lookup-strtab bv (+ str-off (~ (car l) 'name)))
                         name)
               (car l))
             (locate str-off (cdr l)))))
  (let-with fileheader (shstrndx shentsize shoff shnum)
    (define v (vector-ec (: i shnum)
                         (unpack-elf64le-sectionheader 
                           bv
                           (+ shoff (* shentsize i))))) 
    (define strsection (vector-ref v shstrndx)) 
    (locate (~ strsection 'offset) (vector->list v))))

)
