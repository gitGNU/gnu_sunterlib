; taken and adapted from Oleg Kiselyov's tiff-prober.scm
; Changes
;   ##fixnumm.logior --> bitwise-ior
;   ##fixnum.shl --> arithmetic-shift
;   make-endian-port : switches off buffering
; To be evaluated in an environment binding
;   DEFINE-STRUCTURE to the required syntax (like CL's DEFSTRUCT)
;   CHAR->INTEGER to CHAR->ASCII

(define-structure endian-port port msb-first?)
(set! make-endian-port
      (let ((really-make-endian-port make-endian-port))
        (lambda (port msb-first?)
          (set-port-buffering port bufpol/none) ; work around SEEK bug
          (really-make-endian-port port msb-first?))))

(define (close-endian-port eport)
  (close-input-port (endian-port-port eport)))

; endian-port-set-bigendian! EPORT -> UNSPECIFIED
(define (endian-port-set-bigendian! eport)
  (endian-port-msb-first?-set! eport #t))

;   endian-port-set-littlendian! EPORT -> UNSPECIFIED
(define (endian-port-set-littlendian! eport)
  (endian-port-msb-first?-set! eport #f))

;   endian-port-read-int1:: PORT -> UINTEGER (byte)
(define (endian-port-read-int1 eport)
  (let ((c (read-char (endian-port-port eport))))
    (if (eof-object? c) (error "unexpected EOF")
      (char->integer c))))		; Gambit-specific. Need read-byte
                                        ; sunterlib: c->i bound to char->ascii

;   endian-port-read-int2:: PORT -> UINTEGER
(define (endian-port-read-int2 eport)
  (let* ((c1 (endian-port-read-int1 eport))
	 (c2 (endian-port-read-int1 eport)))
    (if (endian-port-msb-first? eport)
       (bitwise-ior (arithmetic-shift c1 8) c2) ;(+ (* c1 256) c2)
       (bitwise-ior (arithmetic-shift c2 8) c1) ;(+ (* c2 256) c1)
      )))

;   endian-port-read-int4:: PORT -> UINTEGER
(define (endian-port-read-int4 eport)
  (let* ((c1 (endian-port-read-int1 eport))
	 (c2 (endian-port-read-int1 eport))
	 (c3 (endian-port-read-int1 eport))
	 (c4 (endian-port-read-int1 eport)))
    (if (endian-port-msb-first? eport)
      ;; (+ c4 (* 256 (+ c3 (* 256 (+ c2 (* 256 c1))))))
      (if (< c1 64)			; The int4 will fit into a fixnum
	(bitwise-ior
	  (arithmetic-shift
	    (bitwise-ior
	      (arithmetic-shift
		(bitwise-ior (arithmetic-shift c1 8) c2) 8) c3) 8) c4)
	(+ (* 256			; The multiplication will make a bignum
	     (bitwise-ior
	       (arithmetic-shift
		 (bitwise-ior (arithmetic-shift c1 8) c2) 8) c3))
	  c4))
      ;; (+ c1 (* 256 (+ c2 (* 256 (+ c3 (* 256 c4))))))
      ; c4 is the most-significant byte
      (if (< c4 64)
	(bitwise-ior
	  (arithmetic-shift
	    (bitwise-ior
	      (arithmetic-shift
		(bitwise-ior (arithmetic-shift c4 8) c3) 8) c2) 8) c1)
	(+ (* 256
	     (bitwise-ior
	       (arithmetic-shift
		 (bitwise-ior (arithmetic-shift c4 8) c3) 8) c2))
	  c1)))))

;   endian-port-setpos PORT INTEGER -> UNSPECIFIED
(define (endian-port-setpos eport pos)
  (OS:fseek-abs (endian-port-port eport) pos))
