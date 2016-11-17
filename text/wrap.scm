;------------------------------------------------------------------------------
; omg.text.wrap : test wrapping function
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

(define-module omg.text.wrap
  (use omg.math.operators)
  (export wrap-text))

(select-module omg.text.wrap)

;------------------------------------------------------------------------------
; Functions
;------------------------------------------------------------------------------

; Wrap Text

(define wrap-text
  (letrec ((find-wrap-index
	     (lambda (str cols)
	       (let ((str-length (dec (string-length str))))
		 (cond ((zero? cols) #f)
		       ((<= str-length cols) str-length)
		       ((eq? (string-ref str cols) #\space) cols)
		       (else (find-wrap-index str (dec cols))))))))
    (lambda (str indent cols)
      (let ((wrap-index (find-wrap-index str (- cols indent))))
	(if (or (< (string-length str) cols) (not wrap-index))
	  (string-append 
	    (make-string indent #\space) 
	    str)
	  (string-append
	    (make-string indent #\space)
	    (substring str 0 wrap-index)
	    (string #\newline)
	    (wrap-text 
	      (substring str (inc wrap-index) (string-length str)) 
	      indent
	      cols)))))))

;------------------------------------------------------------------------------

(provide "omg/text/wrap")

;------------------------------------------------------------------------------
