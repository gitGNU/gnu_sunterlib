; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; generic sequence procedures -- no explicit dispatch on sequence type
;;; 
;;; The code should work with the names of the elementary sequence 
;;; operations bound to the umbrella procedures that dispatch on the
;;; sequence type, or to the specific procedures of a particular type,
;;; 
;;; sequence->list
;;; sequence-fill!
;;; subsequence
;;; sequence-copy
;;; sequence-append
;;; sequence-map
;;; sequence-for-each
;;; sequence-fold
;;; sequence-every
;;; sequence-every/bounds

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


(define (sequence-copy s)
  (subsequence s 0 (sequence-length s)))


(define (sequence-fold/3 kons nil s)
  (let ((end (sequence-length s)))
    (let loop ((subtotal nil) (i 0))
      (if (= i end) subtotal
          (loop (kons (sequence-ref s i) subtotal) (+ i 1))))))


(define (sequence-fold kons nil seq . seqs)
  (if (null? seqs)
      (sequence-fold/3 kons nil seq)
      (let* ((ss (cons seq seqs))
             ;; are we morally obliged to use a fold/3 here?
             (end (apply min (map sequence-length ss))))
        (let loop ((subtotal nil) (i 0))
          (if (= i end) subtotal
              (loop (apply kons (append! (map (lambda (s)
                                                (sequence-ref s i))
                                              ss)
                                         (list subtotal)))
                    (+ i 1)))))))
                                         
                           
(define (sequence-append . seqs)
  (if (null? seqs) (vector)
      (let* ((len (apply + (map sequence-length seqs)))
             (res (make-another-sequence (car seqs) len)))
        (let loop ((ss seqs) (start 0))
          (if (null? ss) res
              (let* ((s (car ss)) (end (sequence-length s)))
                (do ((i 0 (+ i 1)))
                    ((= i end) (loop (cdr ss) (+ start end)))
                  (sequence-set! res (+ start i)
                                 (sequence-ref s i)))))))))


(define (sequence-for-each proc seq . seqs)
  (let* ((ss (cons seq seqs))
         (end (apply min (map sequence-length ss))))
    (do ((i 0 (+ i 1)))
        ((= i end) (unspecific))
      (apply proc (map (lambda (s) (sequence-ref s i)) ss)))))


(define (sequence-map proc seq . seqs)
  (let* ((ss (cons seq seqs))
         (end (apply min (map sequence-length ss)))
         (res (make-another-sequence seq end)))
    (do ((i 0 (+ i 1)))
        ((= i end) res)
      (sequence-set! res i (apply proc (map (lambda (s) (sequence-ref s i))
                                            ss))))))


(define (sequence-every pred . seqs) 
  (if (null? seqs) #t
      (let ((end (apply min (map sequence-length seqs))))
        (let loop ((i 0))
          (cond ((= i end) #t)
                ((apply pred (map (lambda (seq) (sequence-ref seq i))
                                  seqs))
                 (loop (+ i 1)))
                (else #f))))))


(define (sequence-every/bounds start end pred . seqs) 
  (assert (<= 0 start end))
  (if (null? seqs) #t
      (let ((eff-end (apply min end (map sequence-length seqs))))
        (let loop ((i start))
          (cond ((= i eff-end) #t)
                ((apply pred (map (lambda (seq) (sequence-ref seq i))
                                  seqs))
                 (loop (+ i 1)))
                (else #f))))))









