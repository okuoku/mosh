;; NB: This library must be compatible with psyntax-mosh
(library (nmosh internal buildsystem gen-stubs)
         (export
           make-repository
           repository-generate
           )
         (import (yuni util tables scheme)
                 (nmosh pffi signatures)
                 (rnrs)
                 (yuni core) (yuni util files)
                 (only (srfi :1) delete-duplicates!)
                 (srfi :8)
                 (srfi :48)
                 (mosh pp) 
                 (irregex)
                 (shorten))

(define irx-int (irregex 
                  `(: bos (* space) 
                           "NMOSH_EXPORT_SYMBOL_INT("
                           ($ (* (~ #\))))
                           ")")))

(define irx-plugin (irregex 
                     `(: bos (* space) 
                              "NMOSH_EXPORT_SYMBOL_PLUGIN("
                              ($ (* (~ #\))))
                              ")")))

(define irx-pointer (irregex 
                      `(: bos (* space) 
                               "NMOSH_EXPORT_SYMBOL_POINTER("
                               ($ (* (~ #\))))
                               ")")))
(define irx-pointer-int (irregex 
                          `(: bos (* space) 
                            "NMOSH_EXPORT_SYMBOL_POINTER_INT("
                            ($ (* (~ #\))))
                            ")")))

(define (extract-c-exports filename) ;; => int pointer plugin
  (define lines* (file->string-list filename))
  (define irx* (list irx-int irx-pointer irx-pointer-int irx-plugin))
  (define (proc irx)
    (fold-left
      (^[cur line]
        ;; FIXME: Use irregex-match
        (let ((m (irregex-search irx line)))
          (if (irregex-match-data? m)
            (cons (string->symbol (irregex-match-substring m 1)) cur)
            cur)))
      '()
      lines*))
  (define (fold-result int ptr ptr-int plg)
    (values
      int
      (append ptr ptr-int)
      plg))
  (apply fold-result (map proc irx*)))


(define* constant (name value type))
(define* repository (libs funcnames signatures))

(define (make-repository basedir*)
  (define (locate-Library)
    (define libs '())
    (define (add-lib! lib)
      (set! libs (cons lib libs)))
    (for-each (^e (directory-walk 
                    e 
                    (^p (when (string=? (path-basename p) "Library.scm")
                          (add-lib! p)))))
              basedir*)
    libs)
  (make repository
        (libs (locate-Library))
        (funcnames (make-eq-hashtable))
        (signatures '())))

(define* (repository-generate (repository) callstub embed-libs/hdr embed-libs)
  (define libs (~ repository 'libs))
  (define funcnames (~ repository 'funcnames))
  (define signatures (~ repository 'signatures))

  (define (register-funcname plugin-name libname sym)
    (let ((e (hashtable-ref funcnames libname #f)))
      (if e
        (hashtable-set! funcnames libname (cons sym e))
        (hashtable-set! funcnames libname (cons sym (list plugin-name))))))

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
    (define (retptr ret)
      ;; Use proper width
      (cond
        ((string=? "int" ret) "intptr_t")
        (else ret)))
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
                (retptr ret) (gen-arg arg* #t))) ))

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
      (define (with-plgname+functions iter)
        ;; Fold plgname and functions again
        (define ht (make-eq-hashtable))
        (for-each
          (^[p f]
            (let ((c (hashtable-ref ht p '())))
              (hashtable-set! ht p (append f c))))
          plgnames funcs*)
        (receive (pv fv) (hashtable-entries ht)
          (define (append-init p l)
            (if p
              (let ((n (string->symbol
                         (string-append "nmosh_plugin_init_"
                                        (symbol->string p)))))
                (cons n l))
              l))
          (define p* (vector->list pv))
          (define f* (map append-init p* (vector->list fv)))
          (for-each 
            (^[p f]
              ;; FIXME: Do something for force-embedded?
              ;; Do not execute this if force-embedded plugin
              (when p
                (iter p f)))
            p* f*)))
      (when (file-exists? embed-libs/hdr)
        (delete-file embed-libs/hdr))
      (when (file-exists? embed-libs)
        (delete-file embed-libs))
      (call-with-output-file
        embed-libs/hdr
        (^p
          ;; Emit header
          (with-plgname+functions
            (^[pn f*]
              (format p "#ifdef NMOSHPLUGIN_~a_EMBED\n"
                      pn)
              (for-each (^e (format p "extern \"C\" void* ~a(void);\n" e)) f*)
              (gen-libdata p pn pn f*)
              (format p "#endif\n")))))
      (call-with-output-file
        embed-libs
        (^p
          ;; Emit launch
          (with-plgname+functions
            (^[pn bogus]
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
    (define c-exports-int* '())
    (define c-exports-pointer* '())
    (define c-exports-plugin* '())
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
      (string->symbol (format "%plugin-exports-~a" (calc-1-name libname))))
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
                       `(,(calc-export-exports-name myname))
                       '()) 
                   ,@(append c-exports-int* c-exports-pointer*)
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
      ;; Emit int/pointer export
      (let ((x (if parent-libname
                 (calc-export-exports-name parent-libname)
                 (calc-export-exports-name myname))))
        (for-each (^[name]
                    (pp `(define ,name (pickup-export ',name ,x))
                        p))
                  (append c-exports-int*
                          ;; FIXME: Ignore plugin export for now
                          ;; c-exports-plugin*
                          c-exports-pointer*)))

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

    ;; collect c-imports
    (for-each (^e (let ((name (table-metadata-ref e 'c-import:)))
                    (when name (set! c-imports (if (pair? name)
                                                 name
                                                 (list name))))))
              table*)

    ;; Emit c-exports
    (when (pair? c-imports)
      (for-each
        (^[f]
          (define c-file (path-append (path-dirname pth) f))
          (receive (int* pointer* plugin*) (extract-c-exports c-file)
            (pp (list 'int: int* 'pointer: pointer* 'plugin: plugin*))
            (set! c-exports-int* (append c-exports-int* int*))
            (set! c-exports-pointer* (append c-exports-pointer* pointer*))
            (set! c-exports-plugin* (append c-exports-plugin* plugin*))))
        c-imports))

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
  (gen-embed-libs))

)
