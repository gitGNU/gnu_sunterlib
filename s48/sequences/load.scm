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

;;; refers to structures from sunterlib/s48/krims
;;; relies on implicit shadowing of exported bindings

;; sequences as data + behaviour
(define-structure absequences absequences-face
  (open srfi-9+                         ; define-record-type -discloser
        krims                           ; assert
        let-opt                         ; let-optionals [ from scsh ]
        scheme)
  (files uniseqs))

;; specialists for lists, vectors, strings
(define-structure sequence-specifics sequence-specifics-face
  (open krims                           ; assert
        srfi-1+                         ; list procs
        srfi-13                         ; string procs
        let-opt                         ; let-optionals [ from scsh ]
        (modify scheme (hide map for-each member assoc) ; srfi-1+
                       (hide string->list string-copy string-fill!) ; srfi-13
                ))
  (files specseqs))

;; basic sequence accessors etc.
(define-structure sequence-basics sequence-basics-face
  (open krims                           ; gen-dispatch
        let-opt                         ; :optional [ from scsh ]
        sequence-specifics              ; list-set! make-list
        absequences
        byte-vectors
        srfi-1                          ; make-list
        srfi-23                         ; error
        (modify scheme (hide map for-each member assoc) ; srfi-1
                ))
  (files baseqs))

;; sequence operations defined in terms of the basic protocol
(define-structure sequence-extras sequence-extras-face
  (open sequence-basics
        krims                           ; assert
        util                            ; unspecific
        srfi-1+                         ; append! rest
        srfi-23                         ; error
        let-opt                         ; let-optionals [ from scsh ]
        (modify scheme (hide map for-each member assoc) ; srfi-1+
                ))
  (files genseqs))



;; sequence procedures specialised to vectors
(define-structure vector-lib vector-lib-face
  (open krims                           ; assert
        util                            ; unspecific
        let-opt                         ; let-optionals [ from scsh ]
        srfi-1+                         ; append! drop first rest
        (modify scheme (hide map for-each member assoc) ; srfi-1+
                ))
  ;; bind the basic operations to vector specialists
  (begin
    (define sequence? vector?)
    (define sequence-length vector-length)
    (define sequence-ref vector-ref)
    (define sequence-set! vector-set!)
    (define (make-another-sequence v k . maybe-fill)
      (apply make-vector k maybe-fill)))
  (files genseqs     ; generic code
         vecnames)   ; renames stuff, defines constructors
)


;; elementary and other general sequence operations, typically dispatching
;; early on the sequence type in order to make use of built-ins or special
;; code (notably for lists)
(define-structure sequence-lib (compound-interface sequence-basics-face
                                                   sequence-extras-face
                                                   absequences-face)
  (open (with-prefix sequence-extras contiguous-)
        sequence-basics
        absequences
        sequence-specifics
        vector-lib
        srfi-1                          ; list procs
        srfi-13                         ; string procs
        byte-vectors
        let-opt                         ; let-optionals [ from scsh ]
        (modify scheme (hide map for-each member assoc) ; srfi-1
                       (hide string->list string-copy string-fill!) ; srfi-13
                       (hide vector-fill! list->vector) ; vector-lib
                       ))
  (files composeqs))
