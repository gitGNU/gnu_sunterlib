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
(define-record-type :sequence-behavior
  (make-sequence-behavior maker predicate getter setter meter)
  sequence-behavior?
  (maker sequence-behavior:maker)
  (predicate sequence-behavior:predicate)
  (getter sequence-behavior:getter)
  (setter sequence-behavior:setter)
  (meter sequence-behavior:meter))

;; underlying sequence data + behavioral sequence type
(define-record-type :absequence
  ;; avoiding the make-absequence namning pattern for good reason
  (make-absequence-record behavior data)
  absequence?
  (behavior absequence:behavior)
  (data absequence:data))

(define (absequence-ref s k)
  ((sequence-behavior:getter (absequence:behavior s))
   (absequence:data s) k))

(define (absequence-set! s k x)
  ((sequence-behavior:setter (absequence:behavior s))
   (absequence:data s) k x))

(define (absequence-length s)
  ((sequence-behavior:meter (absequence:behavior s))
   (absequence:data s)))

(define (make-absequence/behavior sb k . maybe-fill)
  (make-absequence-record sb
                                (apply (sequence-behavior:maker sb)
                                       k maybe-fill)))

(define (list->absequence/behavior sb xs . opts)
  (let-optionals opts ((start 0) (end (length xs)))
    (assert (<= 0 start end))
    (let ((s (make-absequence/behavior sb (- end start))))
      (do ((i 0 (+ i 1))
           (xs xs (cdr xs)))
          ((= i end) s)
        (absequence-set! s (- i start) (car xs)))))) 

(define (absequence/behavior sb . args)
  (list->absequence/behavior sb args))


(define-record-discloser :absequence
  (lambda (r)
    (let ((sq (absequence:data r)))
      (if (or (vector? sq)
              (string? sq)
              (pair? sq))
          `(absequence:data ,sq)
          `(absequence)))))
