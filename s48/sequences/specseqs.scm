; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

;;; some sequence procedures for specific types (for lists, actually)
;;; list-set!  sublist  list-fill!

;; unspecified return value as usual
(define (list-set! xs k x)
  (set-car! (drop xs k) x))

(define (sublist xs start end)
  (take (drop xs start) (- end start)))

;; unspecified return value -- no [start end] for now
; (define (list-fill! xs x)
;   (pair-for-each (lambda (p) (set-car! p x)) xs))

;; unspecified return value
(define (list-fill! xs x . opts)
  (let-optionals* opts ((start 0 (<= 0 start))
                        ;; the default value is only used to make the
                        ;; check work.  Don't want to compute xs' length.
                        (end start (<= start end) end-supplied?))
    (let loop ((xs xs) (i 0))
      (cond ((null? xs)
             (assert (or (not end-supplied?)
                         (= i end))
                     list-fill!))
            ((< i start)
             (loop (rest xs) (+ i 1)))
            ((if end-supplied? (< i end) #t)
             (set-car! xs x)
             (loop (rest xs) (+ i 1)))))))
