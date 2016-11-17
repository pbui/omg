;------------------------------------------------------------------------------
; $Id: httpd.scm,v 1.1 2005/08/15 07:19:27 pbui Exp pbui $
;------------------------------------------------------------------------------

#|

Copyright (c) Peter Bui. All Rights Reserved.

For specific licensing information, please consult the COPYING file that comes
with the software distribution.  If it is missing, please contact the author at
peter.j.bui@gmail.com.

|#

;------------------------------------------------------------------------------
; HTTPD Module
;------------------------------------------------------------------------------

(define-module omg.rfc.httpd
  (use gauche.net)
  (use gauche.regexp)
  (use rfc.uri)
  (use omg.ext.symbol)
  (export-all))

(select-module omg.rfc.httpd)

;------------------------------------------------------------------------------
; HTTPD Class Definition
;------------------------------------------------------------------------------

(define-class <httpd> ()
  ((environment
     :init-keyword  :table
     :init-value    (make-hash-table)
     :accessor	    httpd-environment)
   (log-port
     :init-keyword  :log-port
     :init-value    (current-output-port))
   (resources
     :init-keyword  :resources
     :init-value    '())
   (state
     :init-keyword  :state
     :init-value    'stop)))

(define-method parse-request ((<hash-table> table) (<string> request))
  (let ((request-list (string-split request " ")))
    (hash-table-put! '

;------------------------------------------------------------------------------

(provide "omg/rfc/httpd")

;------------------------------------------------------------------------------
