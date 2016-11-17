;------------------------------------------------------------------------------
; omg.math.logic: logic functions
;------------------------------------------------------------------------------

; Copyright (c) Peter Bui. All Rights Reserved.

; For specific licensing information, please consult the COPYING file that
; comes with the software distribution.  If it is missing, please contact the
; author at peter.j.bui@gmail.com.

;------------------------------------------------------------------------------

(define-module omg.math.logic
  (export
    binary-combinations
    ))

(select-module omg.math.logic)

;------------------------------------------------------------------------------

(define (binary-combinations n)
  (define (bc n s)
    (if (zero? n)
      s
      (bc (- n 1)
	  (append (map (cut cons 0 <>) s)
		  (map (cut cons 1 <>) s)))))
  (bc n (list '())))

;------------------------------------------------------------------------------

(provide "omg/math/logic")

;------------------------------------------------------------------------------
