; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; generic sequence procedures -- no explicit dispatch on sequence type
;;; 
;;; The code should work with the names of the elementary sequence 
;;; operations bound to the umbrella procedures that dispatch on the
;;; sequence type, or to the specific procedures of a particular type,
;;; 
;;; sequence->list
;;; sequennce-fill!
;;; subsequence
;;; every/bounds

(define (sequence->list s)
  (let loop ((i (sequence-length s)) (xs '()))
    (if (= 0 i) xs
        (loop (- i 1) (cons (sequence-ref s (- i 1)) xs)))))

;; unspecified return value as usual
(define (sequence-fill! s x)
  (let ((len (sequence-length s)))
    (let loop ((i 0))
      (if (< i len)
          (begin 
            (sequence-set! s i x)
            (loop (+ i 1)))))))
        

(define (subsequence s start end)
  (let* ((len (- end start))
         (ss (make-another-sequence s len)))
    (do ((i 0 (+ i 1)))
        ((= i len) ss)
      (sequence-set! ss i (sequence-ref s (+ start i))))))

(define (every/bounds start end pred . args) 
  (assert (<= 0 start end))
  (let ((eff-end (apply min end (map sequence-length args))))
    (let loop ((i start))
      (cond ((= i eff-end) #t)
            ((apply pred (map (lambda (s) (sequence-ref s i)) args))
             (loop (+ i 1)))
            (else #f)))))









