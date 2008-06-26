(define *free-vars*
  `(
    (number? ,number?)
    (cons ,cons)
    (car ,car)
    (cdr ,cdr)
    (null? ,null?)
    (set-car! ,set-car!)
    (set-cdr! ,set-cdr!)
    (sys-display ,display)
    (rxmatch ,rxmatch)
    (regexp? ,regexp?)
    (regexp->string ,regexp->string)
    (rxmatch-start ,rxmatch-start)
    (rxmatch-end ,rxmatch-end)
    (rxmatch-after ,rxmatch-after)
    (rxmatch-before ,rxmatch-before)
    (rxmatch-substring ,rxmatch-substring)
    (make-string ,make-string)
    (string-set! ,string-set!)
    (string-length ,string-length)
    (string->symbol ,string->symbol)
    (string->number ,string->number)
    (string-append ,string-append)
    (string-split ,string-split)
    (number->string ,number->string)
    (reverse ,reverse)
    (eof-object? ,eof-object?)
    (read-char ,read-char)
    (char=? ,char=?)
    (string? ,string?)
    (sys-getenv ,sys-getenv)
    (equal? ,equal?)
    (open-string-input-port ,open-input-string)
    (sys-open-output-string ,open-output-string)
    (sys-port-seek ,port-seek)
    (open-output-file ,open-output-file)
    (close-output-port ,close-output-port)
    (digit->integer ,digit->integer)
    (get-remaining-input-string ,get-remaining-input-string)
    (sys-readdir ,sys-readdir)
    (file-exists? ,file-exists?)
    (sys-get-output-string ,get-output-string)
    (string->regexp ,string->regexp)
    (char->integer ,char->integer)
    (integer->char ,integer->char)
    (format ,format)
    (current-input-port ,current-input-port)
    (current-output-port ,current-output-port)
    (set-current-input-port! ,current-input-port)
    (set-current-output-port! ,current-output-port)
    (char? ,char?)
    (write ,write)
    (gensym ,gensym)
    (string=? ,string=?)
    (vector? ,vector?)
    (list? ,list?)
    (memq ,memq)
    (eq? ,eq?)
    (member ,member)
    (boolean? ,boolean?)
    (symbol->string ,symbol->string)
    (string-ref ,string-ref)
    (error ,error)
    (get-timeofday ,(lambda () (receive (a b) (sys-gettimeofday) (cons a b))))
    (make-eq-hashtable ,(lambda a (make-hash-table 'eq?)))
    (hashtable-set! ,hash-table-put!)
    (hashtable-ref ,hash-table-get)
    (hashtable-keys ,hash-table-keys)
;    (command-line ,command-line)
    (current-error-port ,current-error-port)
    (values ,(lambda () (error "values proc not implemented")))
    (vm/apply ,vm/apply)
    (pair? ,pair?)
    (init-library-table, init-library-table) ;; for test
    (map10, map)
    (find10, find)
    (make-custom-binary-input-port, (lambda (id read! get-position set-position! close) (display "make-custom-binary-input-port not implemented")))
    (get-u8 (lambda (port) (display "get-u8 not implemented")))
    (bytevector-u8-set! (lambda (bv i v) (display "bytevector-u8-set! not implemented")))
    (transcoded-port,  (lambda (binary-port transcoder) (display "bytevector-u8-set! not implemented")))
    (utf-8-codec, (lambda () (display "utf-8-codec not implemented")))
    (make-transcoder, (lambda (codec) (display "make-transcoder not implemented")))
    (eof-object, (lambda () (if #f #f)))
    (sys-open-bytevector-output-port, (lambda (transcoder) (display "sys-open-bytevector-output-port not implemented")))
    (sys-get-bytevector ,(lambda (port) (display "sys-get-bytevector not implemented")))
    (bytevector-length ,(lambda (port) (display "bytevector-length not implemented")))
    (bytevector-u8-ref ,(lambda (bv i) (display "bytevector-u8-ref not implemented")))
    (standard-input-port ,standard-input-port)
    (get-bytevector-n ,(lambda (port count) (display "get-bytevector-n not implemented")))
    (utf8->string ,(lambda (port count) (display "utf-8->string not implemented")))
    (open-file-output-port ,open-output-file)
    (open-file-input-port ,open-input-file)
    (close-input-port ,close-input-port)
    (vector ,vector)
    (regexp-replace ,regexp-replace)
    (regexp-replace-all ,regexp-replace-all)
    (source-info ,(lambda (x) #f))
    (errorf ,errorf)
    (eval ,(lambda a (evaluate (car a))))
    (raise ,(lambda (a) (error a)))
    (raise-continuable ,(lambda (a) (error a)))
    (with-exception-handler ,(lambda x (error "with-exception-handler not implemented")))
    (make-vector-type ,(lambda x (error "make-vector-type not implemented")))
    (vector-type? ,(lambda (x) (error "vector-type not implemented")))
    (vector-type-data ,(lambda (x) (error "vector-type-data not implemented")))
    (vector-type-instance-of? ,(lambda (x y)  (error "vector-type-instance-of not implemented")))
    (make-typed-vector ,(lambda a (error "make-typed-vector not implemented")))
    (typed-vector-get-nth ,(lambda a (error "typed-vector-get-nth not implemented")))
    (typed-vector-set-nth ,(lambda a (error "typed-vector-set-nth not implemented")))
    (typed-vector? ,(lambda a (error "typed-vector not implemented")))
    (typed-vector-type ,(lambda a (error "typed-vector-type not implemented")))
    (apply ,apply-proc)
    (mod ,(lambda a (error "mod not implemented")))
    (div ,(lambda a (error "div not implemented")))
    (assq ,assq)
    (exit ,exit)
    (macroexpand-1 ,pass1/macroexpand)
    (memv ,memv)
    (procedure? ,vector?)
    (load ,(lambda e (error "load not implemented")))
    (symbol? ,symbol?)
    (dynamic-wind ,(lambda e (error "dynamic-wind not implemented")))
    (char<=? ,char<=?)
    (char<? ,char<?)
    (char>=? ,char>=?)
    (char>? ,char>?)
    (read ,(lambda e (error "read not implemented")))
    (vector->list ,vector->list)
    (set-source-info! ,(lambda e #f))
    (call-process ,(lambda e (error "call-process not implemented")))
    (%get-closure-name ,(lambda e (error "%get-closure-name not implemented")))
    (append ,append)
    (append2 ,append)
    (appendA ,append) ;; 実験用
    (append! ,append!)
    (pass3/find-free ,pass3/find-free)
    (pass3/find-sets ,pass3/find-sets)
    (%set-union ,%set-union)
    (%set-intersect ,%set-intersect)
    (make-code-builder ,make-code-builder)
    (code-builder-put1! ,code-builder-put1!)
    (code-builder-put2! ,code-builder-put2!)
    (code-builder-put3! ,code-builder-put3!)
    (code-builder-put4! ,code-builder-put4!)
    (code-builder-put5! ,code-builder-put5!)
    (code-builder-append! ,code-builder-append!)
    (code-builder-emit ,code-builder-emit)
    ;;    (pass4/fixup-labels ,pass4/fixup-labels)
;;     (%set-union ,(lambda (l1 l2)
;;                    (define (set-cons x lst1)
;;                      (if (memq x lst)
;;                          lst
;;                          (cons x lst)))
;;                    (define (rec lst1 lst2)
;;                      (cond
;;                       [(null? lst1) lst2]
;;                       [(null? lst2) lst1]
;;                       [else
;;                        (rec (cdr lst1) (set-cons (car lst1) lst2))]))
;;                    (rec l1 l2)))
    ))
