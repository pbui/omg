;------------------------------------------------------------------------------
; omg.ds.record: Record Data Structure
;------------------------------------------------------------------------------

; Copyright (c) 2006 Peter Bui. All Rights Reserved.
;
; This software is provided 'as-is', without any express or implied warranty.
; In no event will the authors be held liable for any damages arising from the
; use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must not claim
; that you wrote the original software. If you use this software in a product,
; an acknowledgment in the product documentation would be appreciated but is
; not required.
;
; 2. Altered source versions must be plainly marked as such, and must not be
; misrepresented as being the original software.  
;
; 3. This notice may not be removed or altered from any source distribution.
;
; Peter Bui <peter.j.bui@gmail.com>

;------------------------------------------------------------------------------
; Record Data Structure Module
;------------------------------------------------------------------------------

(define-module omg.ds.record
  (extend srfi-9)
  (use omg.ext.symbol)
  (use omg.math.operators)
  (export define-record))

(select-module omg.ds.record)

;------------------------------------------------------------------------------
; Record Data Structure
;------------------------------------------------------------------------------

#|

Macro: define-record <name> . fields

This creates a new record type with the specified name, minus the enclosing <>.
A constructor for the record is defined as "make-name" and for each field, a
getter and setter is defined as "name-field" and "name-field!".

Example:

(use omg.ds.record)

(define-record <2d-point> x y)

(define pt (make-2d-point 0 0))

(d pt)

(2d-point-x pt)
(2d-point-y pt)
(2d-point-x! pt 1)
(2d-point-y! pt 1)

|#

(define-macro (define-record name . fields)
  (let* ((type	       (subsymbol name 1 (dec (symbol-length name))))
	 (constructor  (symbol-append 'make- type))
	 (predicate    (symbol-append type '?))
	 (fields-lists (map (lambda (field)
			      (list field
				    (symbol-append type '- field)
				    (symbol-append type '- field '!)))
			    fields)))
    `(define-record-type ,name
       (,constructor ,@fields)
       ,predicate
       ,@fields-lists)))

;------------------------------------------------------------------------------

(provide "omg/ds/record")

;------------------------------------------------------------------------------
