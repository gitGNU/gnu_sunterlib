; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; a uniform framework for sequence as data + behaviour
;;; in other words: mucho indirection here [ could reduce it ]

;; such records represent behavioural sequence types S
;; maker : integer [t] -> S                -- like MAKE-VECTOR
;; predicate : any -> boolean              -- like VECTOR?
;; getter : S integer --> any              -- like VECTOR-REF
;; setter : S integer any --> unspecified  -- like VECTOR-SET!
;; meter : S --> integer                   -- like VECTOR-LENGTH
(define-record-type :sequence-type
  (make-sequence-type maker predicate getter setter meter)
  sequence-type?
  (maker sequence-type:maker)
  (predicate sequence-type:predicate)
  (getter sequence-type:getter)
  (setter sequence-type:setter)
  (meter sequence-type:meter))

;; underlying sequence data + behavioural sequence type
(define-record-type :behaved-sequence
  ;; avoiding the make-behaved-sequence namning pattern for good reason
  (make-behaved-sequence-record type instance)
  behaved-sequence?
  (type behaved-sequence:type)
  (instance behaved-sequence:instance))

(define (behaved-sequence-ref s k)
  ((sequence-type:getter (behaved-sequence:type s))
   (behaved-sequence:instance s) k))

(define (behaved-sequence-set! s k x)
  ((sequence-type:setter (behaved-sequence:type s))
   (behaved-sequence:instance s) k x))

(define (behaved-sequence-length s)
  ((sequence-type:meter (behaved-sequence:type s))
   (behaved-sequence:instance s)))

(define (make-behaved-sequence/type st k . maybe-fill)
  (make-behaved-sequence-record st
                                (apply (sequence-type:maker st)
                                       k maybe-fill)))

(define (list->behaved-sequence/type st xs)
  (let* ((len (length xs))
         (s (make-behaved-sequence/type st len)))
    (do ((i 0 (+ i 1))
         (xs xs (cdr xs)))
        ((null? xs) s)
      (behaved-sequence-set! s i (car xs))))) 

(define (behaved-sequence/type st . args)
  (list->behaved-sequence/type st args))



