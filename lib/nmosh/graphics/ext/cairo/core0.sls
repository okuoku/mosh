(library (nmosh graphics ext cairo core0)
         (export surface-new
                 surface-new/pointer
                 ;surface->png
                 ;png->surface
                 surface-destroy
                 context-new
                 context-destroy
                 context-draw
                 pattern-rgb
                 pattern-rgba
                 pattern-surface
                 pattern-destroy
                 )
         (import (rnrs)
                 (nmosh stubs mosh-cairo)
                 (srfi :8)
                 (nmosh graphics ext cairo drawops0))

;;

(define (surface-new x y) (mc_mem_create_alpha x y))
(define (surface-new/pointer ptr x y pitch) (mc_mem_create_for ptr x y pitch))
(define (surface-destroy s) (mc_surface_destroy s))
(define (context-new s) (mc_context_create s))
(define (context-destroy ctx) (mc_context_destroy ctx))

(define (context-draw ctx l mtxreg)
  (define mtx (or mtxreg (make-bytevector 0)))
  (define mtxcount (or (and mtxreg (/ (bytevector-length mtxreg)
                                      (* 6 8)))
                       0))
  (receive (op v obj objcount) (pack-drawop l)
    (mc_kick ctx op (bytevector-length op) obj objcount
             v (/ (bytevector-length v) 8)
             mtx mtxcount)))

(define (pattern-rgb r g b) (pattern-rgba r g b 1.0))
(define (pattern-rgba r g b a) (mc_pattern_solid r g b a))
(define (pattern-surface s) (mc_pattern_surface s))
(define (pattern-destroy pat) (mc_pattern_destroy pat))

)
