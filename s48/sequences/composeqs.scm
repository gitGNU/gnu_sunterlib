; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; sequence operations definABLE in terms of the elementary operations
;;; with no regard to the concrete sequence type

(define (ident x) x)

(define (sequence->list s)
  (gen-dispatch
   ((string? string->list)
    (byte-vector? contiguous-sequence->list)
    (vector? vector->list)
    (pair? ident)
    (behaved-sequence? contiguous-sequence->list))
   s))


(define (sequence-fill! s x)
  (gen-dispatch
   ((vector? vector-fill!)
    (string? string-fill!)
    (byte-vector? contiguous-sequence-fill!)
    (pair? ident)
    (behaved-sequence? contiguous-sequence-fill!))
   s x))
   

(define (subsequence s start end)
  (cond ((vector? s)
         (subvector s start end))
        ((string? s)
         (substring s start end))
        ((pair? s)
         (sublist s start end))
        (else (contiguous-subsequence s start end))))


(define (sequence-copy s)
  (gen-dispatch
   ((vector? vector-copy)
    (string? string-copy)
    (byte-vector? contiguous-sequence-copy)
    (pair? list-copy)
    (behaved-sequence? contiguous-sequence-copy))
   s))


(define (sequence-append seq . seqs)
  (cond ((vector? seq) (apply vector-append seq seqs))
        ((string? seq) (apply string-append seq seqs))
        ((pair? seq) (apply append seq seqs))
        (else (apply contiguous-sequence-append seq seqs))))


(define (sequence-map proc s . opts)
  (cond ((vector? s)
         (apply vector-map proc s opts))
        ((string? s)
         (apply string-map proc s opts))
        ((and (pair? s) (null? opts))
         (map proc s))
        (else (apply contiguous-sequence-map proc s opts))))


(define (sequence-for-each proc s . opts)
  (cond ((vector? s)
         (apply vector-for-each proc s opts))
        ((string? s)
         (apply string-for-each proc s opts))
        ((and (pair? s) (null? opts))
         (for-each proc s))
        (else (apply contiguous-sequence-for-each proc s opts))))


(define (sequence-fold kons nil s . opts)
  (cond ((vector? s)
         (apply vector-fold kons nil s opts))
        ((string? s)
         (apply string-fold kons nil s opts))
        ((and (pair? s) (null? opts))
         (fold kons nil s))
        (else (apply contiguous-sequence-fold kons nil s opts))))


(define (sequence-fold-right kons nil s . opts)
  (cond ((vector? s)
         (apply vector-fold-right kons nil s opts))
        ((string? s)
         (apply string-fold-right kons nil s opts))
        ((and (pair? s) (null? opts))
         (fold-right kons nil s))
        (else (apply contiguous-sequence-fold-right kons nil s opts))))


(define (sequence-any pred s . opts)
  (cond ((vector? s)
         (apply vector-any pred s opts))
        ((string? s)
         (apply string-any pred s opts))
        ((and (pair? s) (null? opts))
         (any pred s))
        (else (apply contiguous-sequence-any pred s opts))))


(define (sequence-every pred s . opts)
  (cond ((vector? s)
         (apply vector-every pred s opts))
        ((string? s)
         (apply string-every pred s opts))
        ((and (pair? s) (null? opts))
         (every pred s))
        (else (apply contiguous-sequence-every pred s opts))))


;; The following procedures take or accept >1 sequence argument.
;; Therefore we don't dispatch on the sequence type so that we
;; may support mixed sequences: (sequence-append (vector) "abc" '(anton))
(define sequence-append contiguous-sequence-append)
(define sequences-map contiguous-sequences-map)
(define sequences-for-each contiguous-sequences-for-each)
(define sequences-fold contiguous-sequences-fold)
(define sequences-fold-right contiguous-sequences-fold-right)
(define sequences-any contiguous-sequences-any)
(define sequences-every contiguous-sequences-every)





