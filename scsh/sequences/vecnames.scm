; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; `vector names' for the sequence procedures specialised to vectors, and
;;; some constructors or the like:  list->vector  vector-tabulate

(define subvector subsequence)
(define vector-copy sequence-copy)
(define vector-copy! sequence-copy!)
(define vector-fill! sequence-fill!) ; with opt. start & end
(define vector-tabulate! sequence-tabulate!)
(define vector-append sequence-append)
(define vector-map sequence-map)
(define vector-map-into! sequence-map-into!)
(define vector-for-each sequence-for-each)
(define vector-fold sequence-fold)
(define vector-fold-right sequence-fold-right)
(define vector-null? sequence-null?)
(define vector-any sequence-any)
(define vector-every sequence-every)
(define vector= sequence=)
(define vectors-map sequences-map)
(define vectors-map-into! sequences-map-into!)
(define vectors-for-each sequences-for-each)
(define vectors-fold sequences-fold)
(define vectors-fold-right sequences-fold-right)
(define vectors-any sequences-any)
(define vectors-every sequences-every)
(define vectors= sequences=)

;; redefine with opts
(define (list->vector xs . opts)
  (let-optionals opts ((start 0) (end (length xs)))
    (assert (<= 0 start end))
    (let ((v (make-vector (- end start))))
      (do ((i start (+ i 1))
           (ys (drop xs start) (rest ys)))
          ((= i end) v)
        (vector-set! v (- i start) (first ys))))))


(define (vector-tabulate proc len)
  (vector-tabulate! (make-vector len) 0 proc len))
