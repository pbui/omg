;------------------------------------------------------------------------------
; $Id: symbol.scm,v 1.4 2005/08/10 18:25:02 pbui Exp pbui $
;------------------------------------------------------------------------------

#|

Copyright (c) Peter Bui. All Rights Reserved.

For specific licensing information, please consult the COPYING file that comes
with the software distribution.  If it is missing, please contact the author at
peter.j.bui@gmail.com.

|#

;------------------------------------------------------------------------------
; Symbol Extentions Module
;------------------------------------------------------------------------------

(define-module omg.ext.symbol
  (export-all))

(select-module omg.ext.symbol)

;------------------------------------------------------------------------------
; Symbol Functions
;------------------------------------------------------------------------------

#|

Macro: symbol-append init-symbol ...

Similar to string-append, this appends symbols together.

Example:

(use omg.ext.symbol)

(symbol-append 'moogle '!)

|#

(define-syntax symbol-append
  (syntax-rules ()
    ((symbol-append init-symbol ...)
     (string->symbol
       (string-append (symbol->string init-symbol) ...)))))

;------------------------------------------------------------------------------

#| 

Function: symbol-length symbol

This returns the number of characters in the symbol.

Example:

(use omg.ext.symbol)

(symbol-length 'moogle)

|#

(define (symbol-length symbol)
  (string-length (symbol->string symbol)))

;------------------------------------------------------------------------------

#|

Function: subsymbol symbol start end

This returns a new symbol composed of the specified sybmol starting at the
start index and up to the end index.

Example:

(use omg.ext.symbol)

(subsymbol 'moogle 0 3)
(subsymbol 'moogle 1 (symbol-length 'moogle))

|#

(define (subsymbol symbol start end)
  (string->symbol (substring (symbol->string symbol) start end)))

;------------------------------------------------------------------------------

#|

Macro: symbol<? s1 s2 ...

This uses string<? to compare symbols.

Example:

(use omg.ext.symbol)

(symbol< '1 '2 '3)  => #t
(symbol< '1 '2 '1)  => #f

|#

(define-syntax symbol<?
  (syntax-rules ()
    ((symbol<? s1 s2 ...)
     (string<? (symbol->string s1) (symbol->string s2) ...))))

;------------------------------------------------------------------------------

(provide "omg/ext/symbol")

;------------------------------------------------------------------------------
