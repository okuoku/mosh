(import (yuni util tables scheme)
        (nmosh pffi signatures)
        (rnrs)
        (yuni core) (yuni util files)
        (only (srfi :1) delete-duplicates!)
        (srfi :8)
        (srfi :48)
        (mosh pp) 
        (shorten))

(define targets '("src/nmosh"))
(define callstub "src/call-stubs.inc.c")
(define embed-libs/hdr "src/embed-libs.inc.h")
(define embed-libs "src/embed-libs.inc.c")

(define funcnames (make-eq-hashtable))
(define (register-funcname plugin-name libname sym)
  (let ((e (hashtable-ref funcnames libname #f)))
    (if e
      (hashtable-set! funcnames libname (cons sym e))
      (hashtable-set! funcnames libname (cons sym (list plugin-name))))))
(define signatures '())
(define (register-call-signature l)
  (define sym (string->symbol (signature*->string l)))
  (set! signatures (cons sym signatures)))

(define (outstub1 p sym)
  (define (gen-arg l in-param?)
    (define counter 0)
    (define (out x)
      (if in-param?
        (let ((out  (format "*(~a *)(char *)&args[~a]" x counter)))
          (set! counter (+ 1 counter))
          out)
        x))
    (if (null? l) 
      ""
      (receive (port proc) (open-string-output-port)
        (format port "~a" (out (car l)))
        (for-each (^e (format port ", ~a" (out e))) 
                  (cdr l))
        (proc))))
  (receive (arg* ret) (signature->c-arg*+ret (symbol->string sym))
    (format p "typedef ~a (*func_~a_t)(~a);\n"
            ret sym (gen-arg arg* #f))
    (format p "void\n")
    (format p "callstub_~a(func_~a_t func, uint64_t* args, void* ret){\n"
            sym sym)
    (if (string=? ret "void")
      (format p "    func(~a);\n}\n\n"
              (gen-arg arg* #t))  
      (format p "    *(~a*)ret = func(~a);\n}\n\n"
              ret (gen-arg arg* #t))) ))

(define (gen-libdata p csym libname sym*)
  (define (name rest)
    (if (null? rest)
      (format p "    NIL)")
      (let ((sym (car rest))
            (next (cdr rest)))
        (format p "CONS(FN(~a), \\\n" sym)
        (name next)
        (format p ")"))))
  (format p "#define LIBDATA_~a CONS(SYM(\"~a\"), \\\n" csym libname)
  (name sym*)
  (format p "\n"))

(define (gen-embed-libs)
  (receive (kv ev) (hashtable-entries funcnames)
    (define libnames (vector->list kv))
    (define funcs*+plgname (vector->list ev))
    (define funcs* (map (^e (cdr (reverse e))) funcs*+plgname))
    (define plgnames (map (^e (car (reverse e))) funcs*+plgname))
    (define (with-libname+plgname+functions iter)
      (for-each 
        (^[l p f]
          ;; FIXME: Do something for force-embedded?
          ;; Do not execute this if force-embedded plugin
          (when p
            (iter l p f)))
        libnames plgnames funcs*))
    (when (file-exists? embed-libs/hdr)
      (delete-file embed-libs/hdr))
    (when (file-exists? embed-libs)
      (delete-file embed-libs))
    (call-with-output-file
      embed-libs/hdr
      (^p
        ;; Emit header
        (with-libname+plgname+functions
          (^[l pn f*]
            (format p "#ifdef NMOSHPLUGIN_~a_EMBED\n"
                    pn)
            (for-each (^e (format p "extern \"C\" void* ~a(void);\n" e)) f*)
            (gen-libdata p pn pn f*)
            (format p "#endif\n")))))
    (call-with-output-file
      embed-libs
      (^p
        ;; Emit launch
        (with-libname+plgname+functions
          (^[l pn bogus]
            (format p "#ifdef NMOSHPLUGIN_~a_EMBED\n"
                    pn)
            (format p "    tmp = Object::cons(LIBDATA_~a,tmp);\n" pn)
            (format p "#endif\n")))))))

(define (gen-libdata/callstub p sigs)
  (gen-libdata p 'CALL_STUBS "call-stubs" (map (^e (format "callstub_~a" e))
                                               sigs)))

(define (gen-call-stubs)
  ;; Reduce dupes
  (define sigs (delete-duplicates! signatures))

  ;; Generate file
  (when (file-exists? callstub)
    (delete-file callstub))
  (call-with-output-file
    callstub
    (^p 
      (for-each (^s (outstub1 p s)) sigs)
      (gen-libdata/callstub p sigs))))

(define (locate-Library)
  (define libs '())
  (define (add-lib! lib)
    (set! libs (cons lib libs)))
  (for-each (^e (directory-walk 
                  e 
                  (^p (when (string=? (path-basename p) "Library.scm")
                        (add-lib! p)))))
            targets)
  libs)

(define libs (locate-Library))

(define* constant (name value type))

(define (proc pth table*)
  (define filename)
  (define libname)
  (define internal? #f)
  (define myname #f)
  (define plugin-name #f)
  (define my-exports-name #f)
  (define plugin-export-name #f)
  (define parent-libname #f)
  (define parent-import-name #f)
  (define constants '())
  (define exports '())
  (define objects '())
  (define c-imports '())
  (define (calc-library-name libname)
    (if (pair? libname)
      `(nmosh stubs . ,libname)
      `(nmosh stubs ,libname)))
  (define (calc-1-name libname)
    (let ((c (if (pair? libname)
                (fold-left (^[cur e]
                             (string-append cur "-" (symbol->string e)))
                           (symbol->string (car libname))
                           (cdr libname))
                (symbol->string libname))))
      c))
  (define (calc-export-library-name libname)
    (format "%plugin-library-~a" (calc-1-name libname)))
  (define (calc-export-exports-name libname)
    (format "%plugin-exports-~a" (calc-1-name libname)))
  (define (calc-filename libname)
    (format "lib/nmosh/stubs/~a.mosh.sls"
            (if (pair? libname)
              (fold-left (^[cur e]
                           ;; FIXME: Use path-append here.
                           (string-append cur "/" (symbol->string e))) 
                         (symbol->string (car libname)) 
                         (cdr libname))
              libname)))

  (define (prepare-dirs libname)
    (define dir (path-dirname libname))
    (unless (file-exists? dir)
      (let ((p (path-dirname dir)))
        (unless (file-exists? p)
          ;; Create parent directory first.
          (prepare-dirs dir))
        (create-directory dir))))

  (define (add-constant! name value type)
    (add-export! name)
    (set! constants
      (cons (make constant
                  (name name)
                  (value value)
                  (type (if type type 'int)))
            constants)))
  (define (add-export! sym)
    (set! exports (cons sym exports)))
  (define (add-object! sym)
    (set! objects (cons sym objects)))
  (define (for-each-tablesym sym proc) 
    (for-each (^e 
                (when (table-metadata-ref e sym)
                  (proc e))
                proc table*)
              table*))
  (define (collect-constants tbl)
    (table-for-each tbl '(name value type) add-constant!))

  (define (convtype sym)
    (define (pointer? str)
      (char=? #\* (list-ref (reverse (string->list str)) 0)))
    (let ((str (symbol->string sym)))
      (case sym 
        ((int char* int* void* void double) ;; verbatim output
         sym)
        ((fn) 
         ;; callback is not always a callback..
         'callback)
        (else
          (cond
            ((pointer? str)
             'void*)
            (else
              (assertion-violation #f "invalid type" sym)))))))

  (define (output p)
    (define (emit-function tbl)
      (define (emit ret name args)
        (if args
          (register-call-signature (append args (list ret)))
          (register-call-signature (list ret)))
        ;; add function name for embedded plugin
        (register-funcname plugin-name myname name)
        ;; emit function definition
        (pp `(define ,name (pffi-c-function 
                             %library
                             ,(convtype ret) 
                             ,name ,@(if args (map convtype args) '()))) p))
      (table-for-each tbl '(ret name args) emit))
    ;; emit header
    (format p ";; generated from ~a DO NOT EDIT!!\n" pth)
    (format p "(library ~a\n" (calc-library-name myname))
    (pp `(export ,@(if (and plugin-name (not parent-libname))
                     `(,(string->symbol (calc-export-exports-name myname)))
                     '()) 
                 (rename (%library ,(string->symbol
                                      (calc-export-library-name myname))))
                 ,@exports) 
        p)
    (pp `(import (mosh ffi) ;; FIXME: Do we really need this?
                 (rnrs) 
                 ,(if plugin-name
                    '(nmosh ffi pffi-plugin)
                    '(nmosh ffi pffi))
                 ,@(if parent-libname
                     (list
                       (calc-library-name parent-libname))
                     '())) 
        p)

    ;; emit globals (handle for shared-library or pffi)
    (cond
      (parent-import-name
        (format p "\n\n(define %library ~a)\n" parent-import-name))
      (plugin-name
        (format p "\n\n(define %library (make-pffi-ref/plugin '~a))\n" 
                plugin-name))
      (internal?
        (format p "\n\n(define %library (make-pffi-ref '~a))\n" libname))
      (else
        (format p "\n\n(define-ffi-library %library ~a ~a)\n" libname libname)))

    (newline p)

    ;; emit constants
    (let ((bodies (map (^e (let-with e (name value type)
                             (if (eq? type 'void*)
                               `(define ,name (integer->pointer ,value))
                               `(define ,name ,value))))
                       constants)))
      (for-each (^e (pp e p)) bodies))

    (newline p)

    ;; collect and emit functions
    (for-each-tablesym 'c-function-table emit-function)
    (when (and plugin-name (not parent-libname))
      ;; Plugin init call
      (format p "\n(define ~a (plugin-initialize %library 'nmosh_plugin_init_~a))\n\n" 
              (calc-export-exports-name myname) 
              plugin-name)) 
    ;; emit footer
    (display ")\n" p))

  ;; collect internal?
  (for-each-tablesym '*internal* (^ _ (set! internal? #t)))

  ;; collect plugin name
  (for-each (^e (let ((name (table-metadata-ref e 'plugin:)))
                  (when name (set! plugin-name name))))
            table*)

  ;; collect myname
  (for-each (^e (let ((name (table-metadata-ref e 'libname:)))
                  (when name (set! myname name))))
            table*)

  (unless myname
    (assertion-violation #f "Please specify library name"
                         pth))

  (set! filename (calc-filename myname))
  (prepare-dirs filename)

  ;; collect libname
  (for-each (^e (let ((name (table-metadata-ref e 'libname:)))
                  (when name (set! libname name))))
            table*)

  (unless libname
    (assertion-violation #f "Please specify soname"
                         pth))

  ;; collect parent-libname
  (for-each (^e (let ((name (table-metadata-ref e 'parent-libname:)))
                  (when name (set! parent-libname name))))
            table*)
  (when parent-libname
    (set! parent-import-name (calc-export-library-name parent-libname)))

  ;; collect constants
  (for-each-tablesym 'constant-table collect-constants)
  ;; collect objects (opaque structures or typedefs)
  ;; collect functions (pass1: exports)
  (for-each-tablesym 'c-function-table 
                     (^t (table-for-each 
                           t
                           '(name)
                           (^[name]
                             (add-export! name)))))

  ;; output
  (when (file-exists? filename)
    (delete-file filename))
  (call-with-output-file filename output)
  (display filename))

(define (gen-lib pth)
  (format #t "generating library for ~a\n" pth)
  (proc pth (file->table-list pth))
  (display " generated.\n"))

(for-each gen-lib libs)
(gen-call-stubs)
(gen-embed-libs)
