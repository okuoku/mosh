(library (nmosh graphics ext cairo text0)
         (export cairo-query-text-extent
                 cairo-font-create
                 cairo-font-destroy
                 )
         (import (rnrs)
                 (srfi :42)
                 (nmosh pffi util)
                 (nmosh stubs mosh-cairo))

;;

(define (cairo-font-destroy obj)
  (mc_font_destroy obj))

(define (cairo-font-create name bold? italic?)
  (mc_font_create (string->utf8/null name)
                  (if bold? 1 0)
                  (if italic? 1 0)))

(define (cairo-query-text-extent ctx font text)
  ;; => x-bearing y-bearing width height x-advance y-advance
  (define bv (make-bytevector (* 8 6))) ;; Double
  (mc_font_extent_text ctx (string->utf8/null text) font bv)
  (apply values 
         (list-ec (: i 6)
                  (bytevector-ieee-double-native-ref bv (* i 8)))))

)
