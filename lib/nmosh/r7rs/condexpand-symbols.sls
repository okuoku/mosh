(library (nmosh r7rs condexpand-symbols)
         (export *condexpand-symbols* features)
         (import (rnrs))
         
(define (features) *condexpand-symbols*)
(define *condexpand-symbols*
  '(
    ;; SRFI-0
    else
    ;; STD
    r6rs
    r7rs
    ;; MOSH VM
    exact-closed
    ieee-float
    full-unicode
    ratios
    ;; FRONTEND
    nmosh
    mosh
    ;; SRFIs
    srfi-0
    srfi-1
    srfi-2
    srfi-6
    srfi-8
    srfi-9
    srfi-23
    srfi-26
    srfi-26
    srfi-27
    srfi-31
    srfi-33
    srfi-37
    srfi-38
    srfi-39
    srfi-41
    srfi-42
    srfi-43
    srfi-48
    srfi-61
    srfi-64
    srfi-67
    srfi-69
    srfi-78
    srfi-98
    srfi-99
    ; srfi-115
    ))
)
