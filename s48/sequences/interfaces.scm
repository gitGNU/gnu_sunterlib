; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;; the basic protocol including a vanilla constructor
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
          sequence-copy
          sequence-append
          sequence-map sequences-map
          sequence-for-each sequences-for-each
          sequence-fold sequences-fold
          sequence-fold-right sequences-fold-right
          sequence-any sequences-any
          sequence-every sequences-every))

;; specialised sequence operations (for lists, actually)
(define-interface sequence-specifics-face
  (export list-set!
          list-fill!
          sublist
          ))

;; the sequence ADT etc.
(define-interface behaved-sequences-face
  (export make-sequence-type
          sequence-type?
          make-behaved-sequence-record
          behaved-sequence:type
          make-behaved-sequence/type
          behaved-sequence/type
          list->behaved-sequence/type
          behaved-sequence?
          behaved-sequence-ref
          behaved-sequence-set!
          behaved-sequence-length))

;; the basic + extra sequence procedures
;; [ extends the union of SEQUENCE-BASICS- and -EXTRAS-INTERFACE with 
;;   `VECTOR' replacing `SEQUENCE' ] 
(define-interface vector-lib-face
  (export ;; std constructors
          vector
          make-vector
          ;; basics w/o the vanilla constructor
          vector?
          vector-length
          vector-ref
          vector-set!

          ;; extras
          vector->list
          vector-fill!
          subvector
          vector-copy
          vector-append
          vector-map
          vector-for-each
          vector-fold
          vector-fold-right
          vector-any
          vector-every
          vectors-map
          vectors-for-each
          vectors-fold
          vectors-fold-right
          vectors-any
          vectors-every
          ))

