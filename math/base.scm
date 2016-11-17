;------------------------------------------------------------------------------
; $Id: base.scm,v 1.2 2005/08/12 03:25:51 pbui Exp pbui $
;------------------------------------------------------------------------------

; Copyright (c) Peter Bui. All Rights Reserved.

; For specific licensing information, please consult the COPYING file that
; comes with the software distribution.  If it is missing, please contact the
; author at peter.j.bui@gmail.com.

;------------------------------------------------------------------------------

(define-module omg.math.base
  (export
    base->number
    binary->number 
    octal->number 
    hex->number
    number->base
    number->binary
    number->octal
    number->hex
    binary->hex
    hex->binary
    octal->hex
    hex->octal
    octal->binary
    binary->octal
    ))

(select-module omg.math.base)

;------------------------------------------------------------------------------
; Base to Number Functions
;------------------------------------------------------------------------------

#|

Function: base->number arg base

This helper function converts the argument (number, symbol, or string) in the
specified base into the decimal equivalent.

Example:

(use omg.math.base)

(base->number '666 8)	=> 438

|#

(define (base->number arg base)
  (let ((arg-string (cond ((number? arg) (number->string arg))
			  ((symbol? arg) (symbol->string arg))
			  (else arg))))
    (string->number arg-string base)))

;------------------------------------------------------------------------------

#|

Function: binary->number arg
	  octal->number arg
	  hex->number arg

These functions convert the argument in the specified base to the decimal
equivalent.

Example:

(use omg.base.base)

(binary->number '0101)	=> 5
(octal->number '666)	=> 438
(hex->number 'ff)	=> 255

|#

(define (binary->number arg)
  (base->number arg 2))

(define (octal->number arg)
  (base->number arg 8))

(define (hex->number arg)
  (base->number arg 16))

;------------------------------------------------------------------------------
; Number to Base Functions
;------------------------------------------------------------------------------

#|

Function: base->number arg base

This helper function converts the decimal number into the specified base and
returns the result as a string.

Example:

(use omg.math.base)

(number->base 16 8) => 20

|#

(define (number->base arg base)
  (let ((arg-number (base->number arg 10)))
    (number->string arg-number base #t)))

;------------------------------------------------------------------------------

#|

Function: number->binary arg
	  number->octal arg
	  number->hex arg

These functions convert the decimal number to the specified base and returns
the result as a string.

(use omg.math.base)

(number->binary 10) => "1010"
(number->octal 10)  => "12"
(number->hex 10)    => "a"

|#

(define (number->binary arg)
  (number->base arg 2))

(define (number->octal arg)
  (number->base arg 8))

(define (number->hex arg)
  (number->base arg 16))

;------------------------------------------------------------------------------

(define (binary->hex arg)
  (number->hex (binary->number arg)))

(define (hex->binary arg)
  (number->binary (hex->number arg)))

(define (octal->hex arg)
  (number->hex (octal->number arg)))

(define (hex->octal arg)
  (number->octal (hex->number arg)))

(define (octal->binary arg)
  (number->binary (octal->number arg)))

(define (binary->octal arg)
  (number->octal (binary->number arg)))

;------------------------------------------------------------------------------

(provide "omg/math/base")

;------------------------------------------------------------------------------
