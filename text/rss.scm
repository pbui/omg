#!/usr/local/bin/gosh

;-------------------------------------------------------------------------------
; omg.text.rss : simple rss handling functions
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

(define-module omg.text.rss
  (use omg.ds.record)
  (use omg.text.wrap)
  (use srfi-1)
  (export-all))

(select-module omg.text.rss)

;-------------------------------------------------------------------------------
; Item Data Structure
;-------------------------------------------------------------------------------

(define-record <item> title author link description date)

;-------------------------------------------------------------------------------
; Parse Feed Function
;-------------------------------------------------------------------------------

(define (parse-feed feed)
  (with-input-from-file feed
    (lambda ()
      (parse-port (current-input-port) 'nonitem '()))))

;-------------------------------------------------------------------------------
; Parse Port Function
;-------------------------------------------------------------------------------

(define (parse-port port state items)
  (let ((line (read-line port)))
    (if (eof-object? line)
      items
      (cond ((rxmatch #/<item>/i line)
	     (parse-port port 'item (cons (make-item "" "" "" "" "") items)))
	    ((rxmatch #/<item\s.*?>/i line)
	     (parse-port port 'item (cons (make-item "" "" "" "" "") items)))
	    ((and (eq? state 'item)
	       (rxmatch #/<description>.*<\/description>/i line))
	     (let* ((tag  (rxmatch #/<description>(.*)<\/description>/i line))
		    (body (rxmatch-substring tag 1)))
	       (unless (null? items)
		 (item-description! (car items) body))
	       (parse-port port 'item items)))
	    ((and (eq? state 'item) (rxmatch #/<description>/i line))
	     (let* ((tag  (rxmatch #/<description>(.*)/i line))
		    (body (rxmatch-substring tag 1)))
	       (unless (null? items)
		 (item-description! (car items) body))
	       (parse-port port 'description items)))
	    ((and (eq? state 'description)
	       (rxmatch #/.*?<\/description>/i line)
	     (parse-port port 'item items)))
	    ((rxmatch #/.*?<\/item>/i line)
	     (parse-port port 'nonitem items))
	    (else
	      (let* ((tag-matches (rxmatch #/<(.*?)>(.*?)<\/(.*?)>/i line))
		     (tag	  (rxmatch-substring tag-matches 1))
		     (body	  (rxmatch-substring tag-matches 2)))
		(cond ((and (eq? state 'item) tag-matches)
			 (cond ((rxmatch #/author/i tag)
				(item-author! (car items) body))
			       ((rxmatch #/date/i tag)
				(item-date! (car items) body))
			       ((rxmatch #/description/i tag)
				(item-description! (car items) body))
			       ((rxmatch #/link/i tag)
				(item-link! (car items) body))
			       ((rxmatch #/title/i tag)
				(item-title! (car items) body))))
		      ((eq? state 'description)
		       (unless (null? items)
			 (item-description! (car items)
			    (string-append
			      (item-description (car items)) " " line))))))
		(parse-port port state items))))))

;-------------------------------------------------------------------------------

(provide "omg/text/rss")

;-------------------------------------------------------------------------------
; vim: sts=2 sw=2 ts=8
;-------------------------------------------------------------------------------
