;------------------------------------------------------------------------------
; omg.ext.controls : program control extensions
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
; Control Extentions Module
;------------------------------------------------------------------------------

(define-module omg.ext.controls
  (export 
    for-each-line 
    fold-each-line
    switch))

(select-module omg.ext.controls)

;------------------------------------------------------------------------------
; Control Structures
;------------------------------------------------------------------------------

(define (get-io-port opts)
  (cond ((null? opts)
	 (current-input-port))
	((string? (car opts))
	 (open-input-file (car opts)))
	(else (car opts))))

;------------------------------------------------------------------------------

#|

Function: for-each-line func &optional port-or-file

This control structure applies the given function to every line read from the
port.  If the second argument is a string, then a file input port will be
opened.  The current-input-port is the default port.

Examples:

(use omg.ext.controls)

(for-each-line print)

(for-each-line 
  (lambda (line) 
    (print (string-reverse line)))
  (open-input-file "/etc/motd"))

(use file.util)

(for-each-line print (expand-path "~/.login"))

|#

(define (for-each-line func . opts)
  (let ((port (apply get-io-port opts)))
    (port-for-each func (lambda () (read-line port)))))

;------------------------------------------------------------------------------

#|

Function: fold-each-line func &optional port-or-file

Examples:

(use omg.ext.controls)

|#

(define (fold-each-line fn knil . opts)
  (let ((port (apply get-io-port opts)))
    (port-fold fn knil (lambda () (read-line port)))))

;------------------------------------------------------------------------------

#|

Macro: switch (key ...) (((atoms ...) (body ...)) ...) 

This is a cross between C's switch statement and scheme's case construct.  The
implementation is derived from the R5RS case syntax example: memv was changed
to member and instead of quoting the atoms, they are listed.

Examples:

(use omg.ext.controls)

(switch (remainder (sys-random) 20)
  ((0) 'zero)
  ((1 3 5 7 9) 'odd)
  ((2 4 6 8) 'even)
  (else 'not-single-digit))

((switch (even? (sys-random))
   ((#t) (lambda () 'true))
   ((#f) (lambda () 'false))
   (else (lambda () 'wtf))))

|#

(define-syntax switch 
  (syntax-rules (else)
    ((switch (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (switch atom-key clauses ...)))
    ((switch key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((switch key
       ((atoms ...) result1 result2 ...))
     (if (member key (list atoms ...))
       (begin result1 result2 ...)))
    ((switch key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (member key (list atoms ...))
       (begin result1 result2 ...)
       (switch key clause clauses ...)))))

;------------------------------------------------------------------------------

(provide "omg/ext/controls")

;------------------------------------------------------------------------------
