;------------------------------------------------------------------------------
; $Id: list.scm,v 1.1 2005/09/07 06:02:48 pbui Exp pbui $
;------------------------------------------------------------------------------

; Copyright (c) Peter Bui. All Rights Reserved.

; For specific licensing information, please consult the COPYING file that
; comes with the software distribution.  If it is missing, please contact the
; author at peter.j.bui@gmail.com.

;------------------------------------------------------------------------------

(define-module omg.ext.list
  (use srfi-1)
  (use srfi-27)
  (export-all))

(select-module omg.ext.list)

;------------------------------------------------------------------------------
; List Extension Functions
;------------------------------------------------------------------------------

#|

Function:   shuffle input-list 

This function randomly shuffles the input-list.

Example:

(use omg.ext.list)

(shuffle '(1 2 3)) => (3 1 2)

|#

#|
 | OLD VERSION
(define (shuffle input-list)
  (map
    car
    (sort
      (map 
	(lambda (item) 
	  (cons item (random-real))) 
	input-list)
      (lambda (x y) 
	(< (cdr x) (cdr y))))))

 | NAIVE VERSION
(define (shuffle input-list)
  (let* ((pair-list (map (lambda (item) (cons item (random-real))) input-list))
	 (sort-list (sort pair-list (lambda (x y) (< (cdr x) (cdr y))))))
    (map car sort-list)))
|#

(define (shuffle input-list)
  (let ((list-length	(length input-list))
	(input-vector	(list->vector input-list)))
    (for-each
      (lambda (index)
	(let* ((item	    (vector-ref input-vector index))
	       (swap-index  (+ index (random-integer (- list-length index))))
	       (swap-item   (vector-ref input-vector swap-index)))
	  (vector-set! input-vector swap-index item)
	  (vector-set! input-vector index swap-item)))
      (iota list-length))
    (vector->list input-vector)))

;------------------------------------------------------------------------------

#|

Function:   clear input-list

This simply returns the an empty input-list.

Example:

(use omg.ext.list)

(clear '(1 2 3)) => ()

|#

(define (clear input-list)  '())

;------------------------------------------------------------------------------

#|

Function:   list-split input-list nth

This splits the input-list after every nth element.

(use omg.ext.list)

(list-split '(1 2 3 4 5) 2) => ((1 2) (3 4) (5))

|#

(define (list-split input-list nth)
  (if (< (length input-list) nth)
    (list input-list)
    (cons (take input-list nth) (list-split (drop input-list nth) nth))))

;------------------------------------------------------------------------------

(provide "omg/ext/list")

;------------------------------------------------------------------------------
