; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;; odds and ends
(define-structure krims
  (export (assert :syntax)
          (receive/name :syntax)
          (gen-dispatch :syntax))
  (open srfi-28                         ; format
        srfi-23                         ; error
        scheme)
  (files krims))

;; srfi-9 + define-record-discloser
(define-structure srfi-9+
  (export (define-record-type :syntax)
          define-record-discloser)
  (open scheme-level-2 
	(with-prefix define-record-types sys:))
  (begin
    (define-syntax define-record-type
      (syntax-rules ()
	((define-record-type type-name . stuff)
	 (sys:define-record-type type-name type-name . stuff))))
    (define define-record-discloser sys:define-record-discloser)))
