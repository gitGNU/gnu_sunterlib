;;; stream library

;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; Copyright (c) 2002 by Eric Knauel
;;; Copyright (c) 2002 by Matthias Neubauer
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;;;; constructors

(define the-empty-stream
  (delay '()))

;; -> stream
(define (make-empty-stream)
  the-empty-stream)

;; a stream(a) -> stream(a)
(define (make-stream head tail-stream)
  (delay
    (cons head
	  tail-stream)))

;; (() -> a) stream(a) -> stream(a)
(define (make-stream-lazily head-thunk tail-stream)
  (delay
    (cons (head-thunk)
	  tail-stream)))

(define-syntax stream
  (syntax-rules ()
   ((stream ?h ?t) (make-stream-lazily (lambda () ?h) ?t))))

;;;; predicates

;; stream -> bool
(define (stream-empty? s)
  (null? (force s)))

;;;; destructors

;; stream -> a
(define (stream-head stream)
  (car (force stream)))

;; stream -> stream
(define (stream-tail stream)
  (cdr (force stream)))

;;;; the usual suspects

;; CHANGED
;; (a -> b) stream(a) -> stream(b)
(define (stream-map proc stream)
  (delay
    (let ((stream_ (force stream)))
      (cond
       ((null? stream_) stream_)
       (else
	(cons (proc (car stream_))
	      (stream-map proc (cdr stream_))))))))

;; NEW
;; (a -> b) . list(stream(a)) -> stream(b)
(define (stream-zip-with proc . streams)
  (delay
    (let* ((streams_ (map force streams))
	   (finished? 
	    (or (null? streams_)
		(let loop ((streams_ streams_))
		  (and (not (null? streams_))
		       (or (null? (car streams_))
			   (loop (cdr streams_))))))))
      (if finished? 
	  '()
	  (cons (apply proc (map car streams_))
		(apply stream-zip-with proc (map cdr streams_)))))))

;; CHANGED
;; (a ->* ) -> stream(a) ->*
(define (stream-for-each proc stream)
  (if (not (stream-empty? stream))
      (begin
	(proc (stream-head stream))
	(stream-for-each proc (stream-tail stream)))))

;; stream-filter : (a -> bool) stream(a) -> stream(a)
(define (stream-filter pred? stream)
  (delay 
    (let ((stream_ (force stream)))
      (cond
       ((null? stream_) stream_)
       (else
	(let ((head (car stream_)))
	  (if (pred? head)
	      (cons head
		    (stream-filter pred? (cdr stream_)))
	      (force (stream-filter pred? (cdr stream_))))))))))

;; stream-filter-map : (a -> (union b #f)) -> stream(a) -> stream(b)
(define (stream-filter-map proc stream)
  (delay
    (let ((stream_ (force stream)))
	(cond
	 ((null? stream_) stream_)
	 (else
	  (let ((head (proc (car stream_))))
	    (if head
		(cons head
		      (stream-filter-map proc (cdr stream_)))
		(force (stream-filter-map proc (cdr stream_))))))))))

;; CHANGED
;; stream-unfold : (b -> (union (cons a b) #f)) b -> stream(a)
(define (stream-unfold gen-fun start)
  (delay
    (let ((res (gen-fun start)))
      (if res
	  (cons (car res)
		(stream-unfold gen-fun (cdr res)))
	  '()))))

;; NEW
;;
;; this is Richard Bird and Jeremy Gibbon's "stream" from AFP4
;; transforms a stream by alternating between producer and consumer
;;
;; stream-transform : (b -> (cons list(a) b)) ->
;;                    (b c -> b)
;;                    b
;;                    stream(c) ->
;;                    stream(a)
;;
(define (stream-transform producer consumer state stream)
  (delay
    (let* ((ys&state1 (producer state))
	   (ys (car ys&state1))
	   (state1 (cdr ys&state1))
	   (stream_ (force stream))
	   (as1
	    (cond
	     ((null? stream_) (delay stream_))
	     (else
	      (stream-transform producer 
				consumer
				(consumer state1 (car stream_))
				(cdr stream_))))))
      (force (stream-prepend ys as1)))))

;; stream-take : integer stream(a) -> list(a)
(define (stream-take n stream)
  (if (zero? n) '()
      (let ((stream_ (force stream)))
	(if (null? stream_) stream_
	    (cons (car stream_)
		  (stream-take (- n 1)
			       (cdr stream_)))))))

;; NEW
;; stream-drop : integer stream(a) -> stream(a)
(define (stream-drop n stream)
  (if (zero? n) stream
      (delay 
	(let ((stream_ (force stream)))
	  (if (null? stream_) stream_
	      (if (> n 0)
		  (force (stream-drop (- n 1) (cdr stream_)))
		  (error "stream-drop: negative argument")))))))

;; stream-fold-right : (a b -> b) b stream(a) -> b
(define (stream-fold-right kons knil stream)
  (let loop ((stream stream))
    (if (stream-empty? stream)
	knil
	(kons (stream-head stream)
	      (loop (stream-tail stream))))))

;; CHANGED
;; stream-foldr-lazily : (a (promise b) -> (promise b))
;;                       (promise b)
;;                       stream(a) -> 
;;                       (promise b)
(define (stream-fold-right-lazily kons knil stream)
  (delay
    (let ((stream_ (force stream)))
      (cond
       ((null? stream_) (force knil))
       (else
	(force (kons (car stream_) 
		     (stream-fold-right-lazily kons knil (cdr stream)))))))))

;; NEW
;; stream-prepend : list(a) stream(a) -> stream(a)
(define (stream-prepend l s)
  (if (null? l)
      s
      (delay
	(cons (car l) 
	      (stream-prepend (cdr l) s)))))

;; CHANGED
;; list->stream : list(a) -> stream(a)
(define (list->stream l)
  (stream-unfold (lambda (l) 
		   (and (not (null? l))
			l))
		 l))

;; CHANGED
;; stream-from : integer -> stream(integer)
(define (stream-from n)
  (stream-unfold (lambda (s) (cons s (+ s 1))) n))

;; NEW
;; stream-from-to : integer integer -> stream(integer)
(define (stream-from-to n m)
  (stream-unfold 
   (lambda (s) 
     (and (<= s m)
	  (cons s (+ s 1))))
   n))

;; NEW
;; stream-from-then : integer integer -> stream(integer)
(define (stream-from-then n n1)
  (stream-unfold (lambda (s) (cons s (+ s (- n1 n)))) n))

;; NEW
;; stream-from-to : integer integer integer -> stream(integer)
(define (stream-from-then-to n n1 m)
  (stream-unfold 
   (lambda (s) 
     (and (<= s m)
	  (cons s (+ s (- n1 n)))))
   n))

;; NEW
;; stream-ref : stream(a) integer -> a
(define (stream-ref s n)
  (if (zero? n)
      (stream-head s)
      (if (> n 0)
	  (stream-ref (stream-tail s) (- n 1))
	  (error "stream-ref: invalid reference"))))

;; stream-iterate : (a -> a) -> a -> stream(a)
(define (stream-iterate f a)
  (stream-unfold (lambda (s) (cons s (f s))) a))

;; NEW
;; stream-repeat : a -> stream(a)
(define (stream-repeat a)
  (letrec ((s (delay (cons a s))))
    s))

;; NEW 
;; stream-cycle : list(a) -> stream(a)
(define (stream-cycle l)
  (letrec ((s (stream-prepend l s)))
    s))

;; NEW
;; stream-take-while : (a -> boolean) stream(a) -> stream(a)
(define (stream-take-while p stream)
  (delay
    (let ((stream_ (force stream)))
      (cond 
       ((null? stream_) stream_)
       (else
	(let ((head (car stream_)))
	  (if (p head)
	      (cons head (stream-take-while p (cdr stream_)))
	      '())))))))

;; NEW
;; stream-drop-while : (a -> boolean) stream(a) -> stream(a)
(define (stream-drop-while p stream)
  (delay
    (let ((stream_ (force stream)))
      (cond 
       ((null? stream_) stream_)
       (else
	(let ((head (car stream_)))
	  (if (p head)
	      (force (stream-drop-while p (cdr stream_)))
	      stream_)))))))

;; some tests

;(define test-stream-zip-with
;  (stream-zip-with
;   +
;   (stream-from-to 1 10)
;   (stream-from 42)
;   (stream-from 100)))

;(define test-stream-transform
;  (stream-transform 
;   (lambda (state) 
;     (cons (list state (* state state))
;	   (+ state 1)))
;   (lambda (state1 c)
;     (* state1 c))
;   13
;   (stream-from 1)))

