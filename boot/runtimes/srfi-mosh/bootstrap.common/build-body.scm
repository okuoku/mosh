(define (layout-build l)
  (define (layout-flat l)
    (define (step cur e)
      (case (car e)
	((load r6rs) (append cur (map (lambda (f) (list (car e) f)) (cdr e))))
	(else (append cur (list e)))))
    (fold-left step '() l))
  (define (layout-resolve l)
    (define (step cur e)
      (case (car e)
	((r6rs) 
	 (if (pair? (cadr e))
	   (append cur (list (list 'r6rs (library-name->filename (cadr e)))))
	   (append cur (list e))))
	(else (append cur (list e)))))
    (fold-left step '() l))
  (let* ((flat (layout-flat l))
	 (layout (layout-resolve flat)))
    (define (step e)
      (case (car e)
	((r6rs) (ex-6 (cadr e)))
	((r5rs) (apply ex-5 (cdr e)))
	((load) (read-all (cadr e)))))
    (define (itr cur l)
      (if (pair? l)
	(itr (cons (step (car l)) cur) (cdr l))
	(reverse cur)))
    (apply append (itr '() layout))))

(define (output p)
  (display "load & expanding runtime..")(newline)
  (let ((outfile (layout-build layout)))
    ;(dumpdbg)
    ;(dumpsrc outfile)
    (display "compile...")(newline)
    (let ((vec (compile-to-codevector/toplevel outfile)))
      ;(dumpvec vec)
      (display "writing boot image...")(newline)
      (write-cobj 'nmosh p (obj->fasl vec))
      (display "done.")(newline)
      (display "writing boot image(debug symbol)...")(newline)
      (write-cobj 'nmosh_dbg p (obj->fasl *SYMS*))
      (display "done.")(newline))))

(define (build p)
  (display "#define uint8_t unsigned char\n" p)
  (output p)
  (display "extern \"C\" const unsigned char* nmosh_image_ptr = (unsigned char*)&nmosh_image;\n" p)
  (display "extern \"C\" const unsigned int nmosh_image_size = sizeof(nmosh_image); \n" p)
  (display "extern \"C\" const unsigned char* nmosh_dbg_image_ptr = (unsigned char*)&nmosh_dbg_image;\n" p)
  (display "extern \"C\" const unsigned int nmosh_dbg_image_size = sizeof(nmosh_dbg_image); \n" p))


(define (dumpsrc l)
  (when (file-exists? "boot.scm")
    (delete-file "boot.scm"))
  (display "writing expanded boot image to boot.scm")(newline)
  (call-with-output-file
    "boot.scm"
    (lambda (p)
      (put-string p
		  (call-with-string-output-port
		    (lambda (p) (for-each (lambda (e) (pp e p)) l))))))
  (display "done.")(newline))

(define (dumpdbg)
  (when (file-exists? "nmosh.nmosh-dbg")
    (delete-file "nmosh.nmosh-dbg"))
  (display "writing debug symbol(FASL)..")(newline)
  (call-with-port (open-file-output-port "nmosh.nmosh-dbg")
		  (lambda (p)
		    (put-bytevector p (obj->fasl *SYMS*)))))
(define (dumpvec v)
  (when (file-exists? "bootvec.scm")
    (delete-file "bootvec.scm"))
  (display "writing compiled boot image to bootvec.scm")(newline)
  (call-with-output-file 
    "bootvec.scm" 
    (lambda (p) 
      (vector-for-each 
	(lambda (e) (write e p) (newline p))
	v)))
  (display "done.")(newline))

(when (file-exists? "nmosh_image.cpp")
  (delete-file "nmosh_image.cpp"))
(call-with-output-file "nmosh_image.cpp" build)
