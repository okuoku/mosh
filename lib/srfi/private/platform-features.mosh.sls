(library (srfi private platform-features)
  (export
    expand-time-features
    run-time-features)
  (import
   (only (rnrs) define quote)
    (only (mosh) host-os)
    (srfi private OS-id-features))

  (define (expand-time-features) ;; SRFI-0
    '(mosh))

  (define (run-time-features) ;; SRFI-0?
    (OS-id-features
      (host-os)
      '(("linux" linux posix)
        ("cygwin" linux posix) ;; We will call glibc/newlib as Linux...
        ("darwin" darwin posix)
        ("bsd" bsd linux posix)))) ;; We will call bsdlibc as Linux...
)
