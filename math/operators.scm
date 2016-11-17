;------------------------------------------------------------------------------
; $Id: operators.scm,v 1.1 2005/08/10 19:06:09 pbui Exp pbui $
;------------------------------------------------------------------------------

#|

Copyright (c) Peter Bui. All Rights Reserved.

For specific licensing information, please consult the COPYING file that comes
with the software distribution.  If it is missing, please contact the author at
peter.j.bui@gmail.com.

|#

;------------------------------------------------------------------------------

(define-module omg.math.operators
  (export-all))

(select-module omg.math.operators)

;------------------------------------------------------------------------------
; Math Operators
;------------------------------------------------------------------------------

#| 

Function: inc number &optional delta

This increments the number by specified delta.  The default delta is 1.

Example:

(use omg.math.operators)

(inc 0)	    => 1
(inc 0 5)   => 5

|#

(define (inc number . opts)
  (let-optionals* opts ((delta 1))
    (+ number delta)))

;------------------------------------------------------------------------------

#| 

Function: dec number &optional delta

This decrements the number by specified delta.  The default delta is 1.

Example:

(use omg.math.operators)

(dec 0)	    => -1
(dec 0 5)   => -5

|#

(define (dec number . opts)
  (let-optionals* opts ((delta 1))
    (- number delta)))

;------------------------------------------------------------------------------

#| 

Function: double number

This doubles the number.

Example:

(use omg.math.operators)

(double 1)  => 2

|#

(define (double number)
  (* number 2))

;------------------------------------------------------------------------------

#| 

Function: triple number

This triples the number.

Example:

(use omg.math.operators)

(triple 1)  => 3

|#

(define (triple number)
  (* number 3))

;------------------------------------------------------------------------------

#| 

Function: half number

This halves the number.

Example:

(use omg.math.operators)

(half 1)    => 1/2

|#

(define (half number)
  (/ number 2))

;------------------------------------------------------------------------------

#| 

Function: neg number

This negates the number.

Example:

(use omg.math.operators)

(neg 1)	    => -1

|#

(define (neg number)
  (* number -1))

;------------------------------------------------------------------------------

(provide "omg/math/operators")

;------------------------------------------------------------------------------
