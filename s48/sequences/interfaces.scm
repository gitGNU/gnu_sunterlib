; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;; the basic protocol + a vanilla constructor
(define-interface sequence-basics-face
  (export sequence?
          sequence-length
          sequence-ref
          sequence-set!
          make-another-sequence))

;; things definable in terms of the basic protocol
(define-interface sequence-extras-face
  (export sequence->list
          sequence-fill!
          subsequence
          every/bounds))

;; specialised versions of sequence operations
(define-interface sequence-specifics-face
  (export make-list
          list-set!
          list-fill!
          sublist))

;; the sequence ADT etc.
(define-interface behaved-sequences-face
  (export make-sequence-type
          make-behaved-sequence-record
          behaved-sequence:type
          make-behaved-sequence
          behaved-sequence?
          behaved-sequence-ref
          behaved-sequence-set!
          behaved-sequence-length))
