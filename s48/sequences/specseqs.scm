; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; sequence procedures for specific types (for lists, actually)
;;; list-set!  sublist  list-fill!

;; unspecified return value as usual
(define (list-set! xs k x)
  (set-car! (drop xs k) x))

(define (sublist xs start end)
  (take (drop xs start) (- end start)))

;; unspecified return value -- no [start end] for now
(define (list-fill! xs x)
  (pair-for-each (lambda (p) (set-car! p x)) xs))
