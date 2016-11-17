#!/usr/local/bin/gosh

;-------------------------------------------------------------------------------
; tcpd.scm: TCP Daemon
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

(define-module omg.net.tcpd
  (use gauche.net)
  (use gauche.selector)
  (export-all))
         
(select-module omg.net.tcpd)

;-------------------------------------------------------------------------------
; TCP Daemon
;-------------------------------------------------------------------------------

(define-class <tcpd> () ())

(define-method generic-accept-handler ((tcpd <tcpd>) port flag)
  (let* ((client    (socket-accept (tcpd-socket tcpd)))
	 (selector  (tcpd-selector tcpd))
	 (iport	    (socket-input-port client)))
    (selector-add! selector 
      (socket-input-port client :buffered? #f)
      (lambda (input flag) ((tcpd-rhandler tcpd) tcpd client iport flag))
      '(r))
    (selector-add! selector 
      (socket-input-port client :buffered? #f)
      (lambda (input flag) ((tcpd-xhandler tcpd) tcpd client iport flag))
      '(x))))

(define-method generic-read-handler ((tcpd <tcpd>) client iport flag)
  (let ((selector (tcpd-selector tcpd))
	(socket	  (tcpd-socket tcpd))
	(oport	  (socket-output-port client)))
    (let ((line (read-line iport)))
      (if (eof-object? line)
	((tcpd-xhandler tcpd) tcpd client iport flag)
	(format oport "~A~%" line)))))

(define-method generic-exception-handler ((tcpd <tcpd>) client iport flag)
  (let ((selector (tcpd-selector tcpd))
	(socket	  (tcpd-socket tcpd)))
    (selector-delete! selector iport #f #f)
    (socket-close client)))

;-------------------------------------------------------------------------------

(define-class <tcpd> ()
  ((port     :init-value 0  :init-keyword :port     :accessor tcpd-port)
   (ahandler :init-value    generic-accept-handler 
	     :init-keyword  :ahandler 
	     :accessor	    tcpd-ahandler)
   (rhandler :init-value    generic-read-handler 
	     :init-keyword  :rhandler 
	     :accessor	    tcpd-rhandler)
   (whandler :init-value '() :init-keyword :whandler :accessor tcpd-whandler)
   (xhandler :init-value    generic-exception-handler 
	     :init-keyword  :xhandler 
	     :accessor	    tcpd-xhandler)
   (sessions :init-value '() :init-keyword :sessions :accessor tcpd-sessions)
   (selector :init-value '() :accessor tcpd-selector)
   (socket   :init-value '() :accessor tcpd-socket)))

;-------------------------------------------------------------------------------

(define-method start-tcpd ((tcpd <tcpd>))
  ; Initialize selector and socket
  (set! (tcpd-selector tcpd) (make <selector>)) 
  (set! (tcpd-socket tcpd) (make-server-socket 'inet (tcpd-port tcpd)))
 
  ; Process socket I/O
  (let ((selector (tcpd-selector tcpd))
	(socket	  (tcpd-socket tcpd)))
  
    ; Register accept-handler
    (selector-add! selector
      (socket-fd socket)
      (lambda (port flag) ((tcpd-ahandler tcpd) tcpd port flag))
      '(r))

    ; Loop forever
    (do () (#f) (selector-select selector))))

;-------------------------------------------------------------------------------

(provide "omg/net/tcpd")

;-------------------------------------------------------------------------------
; vim: sts=2 sw=2 ts=8 ft=scheme
;-------------------------------------------------------------------------------
