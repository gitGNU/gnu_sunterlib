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
;;; sequence-map sequences-map
;;; sequence-for-each sequences-for-each
;;; sequence-fold sequences-fold
;;; sequence-fold-right sequence-fold-right
;;; sequence-any sequences-any
;;; sequence-every sequences-every

(define (id x) x)

(define (sequence->list s . opts)
  (let-optionals opts ((start 0) (end (sequence-length s)))
    (assert (<= 0 start end))
    (let loop ((i end) (xs '()))
      (if (= i start) xs
          (loop (- i 1) (cons (sequence-ref s (- i 1)) xs))))))

;; unspecified return value as usual
(define (sequence-fill! s x . opts)
  (let-optionals opts ((start 0) (end (sequence-length s)))
    (assert (<= 0 start end))
    (let loop ((i 0))
      (if (< i end)
          (begin 
            (sequence-set! s i x)
            (loop (+ i 1)))))))
        

(define (subsequence s start end)
  (assert (<= 0 start end))
  (let* ((len (- end start))
         (ss (make-another-sequence s len)))
    (do ((i 0 (+ i 1)))
        ((= i len) ss)
      (sequence-set! ss i (sequence-ref s (+ start i))))))


(define (sequence-copy s . opts)
  (let-optionals opts ((start 0) (end (sequence-length s)))
    (assert (<= 0 start end))
    (subsequence s start end)))


(define (sequence-fold kons nil s . opts)
  (let-optionals opts ((start 0)
                       (end (sequence-length s)))
    (assert (<= 0 start end))
    (let loop ((subtotal nil) (i start))
      (if (= i end) subtotal
          (loop (kons (sequence-ref s i) subtotal) (+ i 1))))))


(define (sequences-fold kons nil seq . seqs)
  (if (null? seqs)
      (sequence-fold kons nil seq)
      (let* ((ss (cons seq seqs))
             ;; are we morally obliged to use FOLD here?
             (end (apply min (map sequence-length ss))))
        (let loop ((subtotal nil) (i 0))
          (if (= i end) subtotal
              (loop (apply kons (append! (map (lambda (s)
                                                (sequence-ref s i))
                                              ss)
                                         (list subtotal)))
                    (+ i 1)))))))
                                         
                           
(define (sequence-fold-right kons nil s . opts)
  (let-optionals opts ((start 0)
                 (end (sequence-length s)))
    (assert (<= 0 start end))
    (let loop ((subtotal nil) (i end))
      (if (= i start) subtotal
          (loop (kons (sequence-ref s (- i 1)) subtotal) (- i 1))))))


(define (sequences-fold-right kons nil seq . seqs)
  (if (null? seqs)
      (sequence-fold-right kons nil seq)
      (let* ((ss (cons seq seqs))
             ;; are we morally obliged to use FOLD here?
             (end (apply min (map sequence-length ss))))
        (let loop ((subtotal nil) (i (- end 1)))
          (if (= i -1) subtotal
              (loop (apply kons (append! (map (lambda (s)
                                                (sequence-ref s i))
                                              ss)
                                         (list subtotal)))
                    (- i 1)))))))


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


(define (sequence-for-each proc seq . opts)
  (let-optionals opts ((start 0) (end (sequence-length seq)))
    (assert (<= 0 start end))
    (do ((i start (+ i 1)))
        ((= i end) (unspecific))
      (proc (sequence-ref seq i)))))


(define (sequences-for-each proc seq . seqs)
  (let* ((ss (cons seq seqs))
         (end (apply min (map sequence-length ss))))
    (do ((i 0 (+ i 1)))
        ((= i end) (unspecific))
      (apply proc (map (lambda (s) (sequence-ref s i)) ss)))))


(define (sequence-map proc seq . opts)
  (let-optionals opts ((start 0) (end (sequence-length seq)))
    (assert (<= start end))
    (let ((res (make-another-sequence seq end)))
      (do ((i start (+ i 1)))
          ((= i end) res)
        (sequence-set! res i (proc (sequence-ref seq i)))))))


(define (sequences-map proc seq . seqs)
  (let* ((ss (cons seq seqs))
         (end (apply min (map sequence-length ss)))
         (res (make-another-sequence seq end)))
    (do ((i 0 (+ i 1)))
        ((= i end) res)
      (sequence-set! res i (apply proc (map (lambda (s) (sequence-ref s i))
                                            ss))))))

(define (sequence-any foo? seq . opts)
  (let-optionals opts ((start 0) (end (sequence-length seq)))
    (assert (<= 0 start end))
    (let loop ((i start))
      (cond ((= i end) #f)
            ((foo? (sequence-ref seq i)) => id)
            (else (loop (+ i 1)))))))


(define (sequences-any foo? . seqs) 
  (if (null? seqs) #f
      (let ((end (apply min (map sequence-length seqs))))
        (let loop ((i 0))
          (cond ((= i end) #f)
                ((apply foo? (map (lambda (seq) (sequence-ref seq i))
                                  seqs))
                 => id)
                (else (loop (+ i 1))))))))


(define (sequence-every foo? seq . opts)
  (let-optionals opts ((start 0) (end (sequence-length seq)))
    (assert (<= 0 start end))
    (let loop ((i start) (res #t))
      (cond ((= i end) res)
            ((foo? (sequence-ref seq i))
             => (lambda (r) (loop (+ i 1) r)))
            (else #f)))))


(define (sequences-every foo? . seqs) 
  (if (null? seqs) #t
      (let ((end (apply min (map sequence-length seqs))))
        (let loop ((i 0))
          (cond ((= i end) #t)
                ((apply foo? (map (lambda (seq) (sequence-ref seq i))
                                  seqs))
                 (loop (+ i 1)))
                (else #f))))))









