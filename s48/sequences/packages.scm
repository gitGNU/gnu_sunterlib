; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; refers to structure KRIMS from sunterlib/s48/krims

;; sequences as data + behaviour
(define-structure behaved-sequences behaved-sequences-face
  (open srfi-9                          ; define-record-type
        scheme)
  (files uniseqs))

;; some sequence operations tuned for lists
(define-structure sequence-specifics sequence-specifics-face
  (open srfi-1         ; drop first take make-list pair-for-each
        scheme)
  (files specseqs))
        
;; basic sequence accessors etc.
(define-structure sequence-basics sequence-basics-face
  (open krims                           ; gen-dispatch
        let-opt                         ; :optional
        sequence-specifics              ; list-set! make-list
        behaved-sequences
        byte-vectors
        srfi-23                         ; error
        scheme)
  (files baseqs))

;; sequence operations defined in terms of the basic protocol
(define-structure sequence-extras sequence-extras-face
  (open sequence-basics
        krims                           ; assert
        util                            ; unspecific
        srfi-1                          ; append!
        srfi-23                         ; error
        scheme)
  (files genseqs))

;; elementary and other general sequence operations, typically dispatching
;; early on the sequence type in order to make use of built-ins or special
;; code (notably for lists)
(define-structure sequence-lib (compound-interface sequence-basics-face
                                                   sequence-extras-face
                                                   behaved-sequences-face)
  (open krims                           ; gen-dispatch
        (with-prefix sequence-extras contiguous-)
        sequence-basics
        behaved-sequences
        sequence-specifics
        srfi-1                          ; list-copy
        byte-vectors
        scheme)
  (files composeqs))
