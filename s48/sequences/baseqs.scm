; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; general sequences
;;; the sequence protocol -- elementary procedures
;;; sequence predicate, basic accessors, length, and even a constructor

(define (sequence? s)
  (or (string? s)
      (byte-vector? s)
      (vector? s)
      (list? s)
      (behaved-sequence? s)))

(define (sequence-length s)
  (gen-dispatch
   ((string? string-length)
    (byte-vector? byte-vector-length)
    (vector? vector-length)
    (list? length)
    (behaved-sequence? behaved-sequence-length))
   s))


(define (sequence-ref s k)
  (gen-dispatch
   ((string? string-ref)
    (byte-vector? byte-vector-ref)
    (vector? vector-ref)
    (list? list-ref)
    (behaved-sequence? behaved-sequence-ref))
   s k))


(define (sequence-set! s k x)
  (gen-dispatch
   ((string? string-set!)
    (byte-vector? byte-vector-set!)
    (vector? vector-set!)
    (list? list-set!)
    (behaved-sequence? behaved-sequence-set!))
   s k x))


(define (make-another-sequence s len . maybe-fill)
  (cond ((string? s) (apply make-string len maybe-fill))
        ((byte-vector? s) (make-byte-vector len
                                            ;; mbv requires 2nd arg
                                            (:optional maybe-fill 0)))
        ((vector? s) (apply make-vector len maybe-fill))
        ((list? s) (apply make-list len maybe-fill))
        ((behaved-sequence? s)
         (apply make-behaved-sequence
                (behaved-sequence:type s) len maybe-fill))
        (else (error "make-another : unsupported sequence(?) type" s))))  









