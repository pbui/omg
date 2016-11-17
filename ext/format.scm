;------------------------------------------------------------------------------
; $Id: format.scm,v 1.1 2005/08/10 22:14:36 pbui Exp pbui $
;------------------------------------------------------------------------------

; Copyright (c) Peter Bui. All Rights Reserved.

; For specific licensing information, please consult the COPYING file that
; comes with the software distribution.  If it is missing, please contact the
; author at peter.j.bui@gmail.com.

;------------------------------------------------------------------------------
; Format Extentions Module
;------------------------------------------------------------------------------

(define-module omg.ext.format
  (export-all)) 

(select-module omg.ext.format)

;------------------------------------------------------------------------------
; Command Print
;------------------------------------------------------------------------------

#|

Function: cprint command 

The command gets evaluated, then both the command and the result is printed and
the result is returned.

Example:

(use omg.ext.format)

(cprint '(lambda () "who-ha!"))
(cprint '(+ 1 1))

|#

(define (cprint command)
  (let ((result (eval command (interaction-environment))))
    (print command " -> " result)
    result))

;------------------------------------------------------------------------------

(define (fprint port . args)
  (for-each (cut display <> port) args)
  (newline port))

(define (eprint . args)
  (apply fprint (cons (current-error-port) args)))

;------------------------------------------------------------------------------
; Formatted Print Statements
;------------------------------------------------------------------------------

#|

Function: eprintf format-string &optional args

This prints the formatted string to the current-error-port.

Example:

(use omg.ext.format)

(eprintf "Error: ~a~%" 0)

|#

(define (eprintf format-string . args)
  (apply format (append (list (current-error-port) format-string) args)))

;------------------------------------------------------------------------------

#|

Function: fprintf port format-string &optional args

This prints the formatted string to the specified port.

Example:

(use omg.ext.format)

(fprintf (current-input-port) "~a~%" 'halo)

|#

(define (fprintf port format-string . args)
  (apply format (append (list port format-string) args))) 

;------------------------------------------------------------------------------

#|

Function: printf format-string &optional args

This prints the formatted string to the current-output-port.

Example:

(use omg.ext.format)

(printf "~a~%" 'halo)

|#

(define (printf format-string . args)
  (apply format (append (list (current-output-port) format-string) args)))

;------------------------------------------------------------------------------

#|

Function: sprintf format-string &optional args

This returns the formatted string.

Example:

(use omg.ext.format)

(sprintf "~a~%" 'halo)

|#

(define (sprintf format-string . args)
  (apply format (append (list #f format-string) args)))

;------------------------------------------------------------------------------

(provide "omg/ext/format")

;------------------------------------------------------------------------------
