; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; sequence operations definABLE in terms of the elementary operations
;;; with no regard to the concrete sequence type
;;; [ not too much there yet ]

(define (ident x) x)

(define (sequence->list s)
  (gen-dispatch
   ((string? string->list)
    (byte-vector? contiguous-sequence->list)
    (vector? vector->list)
    (list? ident)
    (behaved-sequence? contiguous-sequence->list))
   s))


(define (sequence-fill! s x)
  (gen-dispatch
   ((string? string-fill!)
    (byte-vector? contiguous-sequence-fill!)
    (vector? vector-fill!)
    (list? ident)
    (behaved-sequence? contiguous-sequence-fill!))
   s x))
   

(define (subsequence s start end)
  (cond ((pair? s)
         (sublist s start end))
        ((string? s)
         (substring s start end))
        (else (contiguous-subsequence s start end))))


(define (sequence-copy s)
  (gen-dispatch
   ((string? string-copy)
    (byte-vector? contiguous-sequence-copy)
    (vector? contiguous-sequence-copy)
    (list? list-copy)
    (behaved-sequence? contiguous-sequence-copy))
   s))

;; The following procedures take or accept >1 sequence argument.
;; Therefore we don't dispatch on the sequence type so that we
;; may support mixed sequences: (sequence-append (vector) "abc" '(anton))
(define sequence-append contiguous-sequence-append)
(define sequence-map contiguous-sequence-map)
(define sequence-for-each contiguous-sequence-for-each)
(define sequence-fold contiguous-sequence-fold)
(define sequence-every contiguous-sequence-every)
(define sequence-every/bounds contiguous-sequence-every/bounds)

