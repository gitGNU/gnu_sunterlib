; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; sequence operations definABLE in terms of the elementary operations
;;; [ not much there yet ]

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

;; this is rather inefficient for lists-only uses, but supports mixed
;; sequences (comparing lists against vectors, for instance) 
(define every/bounds contiguous-every/bounds)

