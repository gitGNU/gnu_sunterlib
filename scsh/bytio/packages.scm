; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;; byte (rather than character) i/o
(define-structure bytio bytio-face
  (open krims                           ; assert
        scheme
	byte-vectors 
	ascii				; char<->ascii
	i/o				; read-block
	ports				; port-buffer
	primitives			; copy-bytes!
	let-opt				; let-optionals
	)
  (files rw-bytes))

