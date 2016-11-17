#!/usr/local/bin/gosh

;-------------------------------------------------------------------------------
; omg.text.html : simple html handling functions
;-------------------------------------------------------------------------------

; Copyright (c) 2006 Peter Bui. All Rights Reserved.

; This software is provided 'as-is', without any express or implied warranty.
; In no event will the authors be held liable for any damages arising from the
; use of this software.

; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute it
; freely, subject to the following restrictions:

; 1. The origin of this software must not be misrepresented; you must not claim
; that you wrote the original software. If you use this software in a product,
; an acknowledgment in the product documentation would be appreciated but is
; not required.

; 2. Altered source versions must be plainly marked as such, and must not be
; misrepresented as being the original software.  

; 3. This notice may not be removed or altered from any source distribution.

; Peter Bui <peter.j.bui@gmail.com>

;-------------------------------------------------------------------------------

(define-module omg.text.html
  (export-all))

(select-module omg.text.html)

;-------------------------------------------------------------------------------
; HTML Functions
;-------------------------------------------------------------------------------

; Remove HTML Symbols From String

(define (html-unescape-string str)
  (for-each
    (lambda (pair)
      (set! str (regexp-replace-all (car pair) str (cdr pair))))
    (list '("&lt;" . "<")
	  '("&gt;" . ">")
	  '("&amp;" . "&")
	  '("&quot;" . "\"")
	  '(#/&\#.*?;/ . "")))
  str)

; Strip Tags
(define (html-strip-tags str)
  (regexp-replace-all #/<.*?>/i str ""))

;-------------------------------------------------------------------------------

(provide "omg/text/html")

;-------------------------------------------------------------------------------
; vim: sts=2 sw=2 ts=8
;-------------------------------------------------------------------------------
