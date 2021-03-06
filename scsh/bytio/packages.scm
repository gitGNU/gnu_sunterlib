; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

(define-interface bytio-face
  (export read-byte
	  peek-byte
	  write-byte

          read-bytes!/partial
          read-bytes/partial
          read-bytes!
          read-bytes
          write-bytes/partial
          write-bytes
          ))

;; byte (rather than character) i/o
(define-structure bytio bytio-face
  (open krims                           ; assert
        sequence-lib                    ; subsequence sequence-length
	byte-vectors
	let-opt				; let-optionals
        srfi-23                         ; error
        scsh                            ; fdes & port stuff, bitwise-and
        scheme
	ascii				; char<->ascii
	i/o				; read-block
	ports				; port-buffer
	primitives			; copy-bytes!
	)
  (files rw-bytes))
