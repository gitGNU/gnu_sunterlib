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
          sequence-tabulate!
          sequence-fill!
          subsequence
          sequence-copy
          sequence-copy!
          sequence-copy/maker
          sequence-append
          sequence-map sequences-map
          sequence-map/maker sequences-map/maker
          sequence-map-into! sequences-map-into!
          sequence-for-each sequences-for-each
          sequence-fold sequences-fold
          sequence-fold-right sequences-fold-right
          sequence-null?
          sequence-any sequences-any
          sequence-every sequences-every
          sequence= sequences=
          ))

;; specialised sequence operations (for lists, actually)
(define-interface sequence-specifics-face
  (export list-set!
          list-fill!
          sublist
          ))

;; the sequence ADT etc.
(define-interface absequences-face
  (export make-sequence-behavior
          sequence-behavior?
          make-absequence-record
          absequence:behavior
          make-absequence/behavior
          absequence/behavior
          list->absequence/behavior
          absequence?
          absequence-ref
          absequence-set!
          absequence-length))

;; the basic + extra sequence procedures
;; [ extends the union of SEQUENCE-BASICS- and -EXTRAS-INTERFACE with
;;   `VECTOR' replacing `SEQUENCE' ]
(define-interface vector-lib-face
  (export ;; constructors and the like
          make-vector
          vector
          list->vector                  ; with opts
          vector-tabulate
          ;; basics w/o the vanilla constructor
          vector?
          vector-length
          vector-ref
          vector-set!

          ;; extras
          vector->list
          vector-fill!
          vector-tabulate!
          subvector
          vector-copy
          vector-copy!
          vector-append
          vector-map   ; forget the optional MAKER arg
          vector-map-into!
          vector-for-each
          vector-fold
          vector-fold-right
          vector-null?
          vector-any
          vector-every
          vector=
          vectors-map  ; but not vectors-map/maker
          vectors-map-into!
          vectors-for-each
          vectors-fold
          vectors-fold-right
          vectors-any
          vectors-every
          vectors=
          ))

