; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; refers to structure KRIMS from sunterlib/s48/krims

;; sequences as data + behaviour
(define-structure behaved-sequences behaved-sequences-face
  (open srfi-9                          ; define-record-type
        scheme)
  (files uniseqs))

;; specialists for lists, vectors, strings
(define-structure sequence-specifics sequence-specifics-face
  (open srfi-1                          ; list procs
        srfi-13                         ; string procs
        scheme)
  (files specseqs))
        
;; basic sequence accessors etc.
(define-structure sequence-basics sequence-basics-face
  (open krims                           ; gen-dispatch
        let-opt                         ; :optional [ from scsh ]
        sequence-specifics              ; list-set! make-list
        behaved-sequences
        byte-vectors
        srfi-1                          ; make-list
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
        let-opt                         ; let-optionals [ from scsh ]
        scheme)
  (files genseqs))



;; sequence procedures specialised to vectors
(define-structure vector-lib vector-lib-face
  (open krims                           ; assert
        util                            ; unspecific
        let-opt                         ; let-optionals [ from scsh ]
        srfi-1                          ; append!
        scheme)
  ;; bind the basic operations to vector specialists
  (begin
    (define sequence? vector?)
    (define sequence-length vector-length)
    (define sequence-ref vector-ref)
    (define sequence-set! vector-set!)
    (define (make-another-sequence v k . maybe-fill)
      (apply make-vector k maybe-fill)))
  (files genseqs)
  ;; rename extras not supplied by scheme
  (begin
    (define subvector subsequence)
    (define vector-copy sequence-copy)
    (define vector-append sequence-append)
    (define vector-map sequence-map)
    (define vector-for-each sequence-for-each)
    (define vector-fold sequence-fold)
    (define vector-fold-right sequence-fold-right)
    (define vector-any sequence-any) 
    (define vector-every sequence-every) 
    (define vectors-map sequences-map)
    (define vectors-for-each sequences-for-each)
    (define vectors-fold sequences-fold)
    (define vectors-fold-right sequences-fold-right)
    (define vectors-any sequences-any) 
    (define vectors-every sequences-every) 
    ))


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
        byte-vectors
        vector-lib
        srfi-1                          ; list procs
        srfi-13                         ; string procs
        let-opt                         ; let-optionals [ from scsh ]
        scheme)
  (files composeqs))
