;------------------------------------------------------------------------------
; $Id: string.scm,v 1.1 2005/08/12 15:47:48 pbui Exp pbui $
;------------------------------------------------------------------------------

#|

Copyright (c) Peter Bui. All Rights Reserved.

For specific licensing information, please consult the COPYING file that comes
with the software distribution.  If it is missing, please contact the author at
peter.j.bui@gmail.com.

|#

;------------------------------------------------------------------------------
; String Extentions Module
;------------------------------------------------------------------------------

(define-module omg.ext.string
  (use srfi-13)
  (export-all))

(select-module omg.ext.string)

;------------------------------------------------------------------------------
; String Functions
;------------------------------------------------------------------------------

#|

Macro: string-rscan str del &optional return ...

This is the same as string-scan except that it scans in reverse.

Example:

(use omg.ext.string)

(string-rscan "the.omg.ogg" #\.)    => 7 

|#

(define (string-rscan str del . opts)
  (let-optionals* opts ((return 'index))
    (let ((s-scan (string-scan (string-reverse str) del return)))
      (if s-scan
	(- (string-length str) s-scan 1)
	s-scan))))

;------------------------------------------------------------------------------

(provide "omg/ext/string")

;------------------------------------------------------------------------------
