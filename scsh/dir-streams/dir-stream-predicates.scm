;;; predicates for directory streams

;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; Copyright (c) 2002 by Eric Knauel.
;;; Copyright (c) 2002 by Matthias Neubauer.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define (dir-stream-display ds)
  (dir-stream-for-each ds display display))

(define (fs-object-size-less-then? fs-object size)
  (< (file-info:size (fs-object-info fs-object)) size))

(define (fs-object-size-greater-then? fs-object size)
  (> (file-info:size (fs-object-info fs-object)) size))

(define (minutes->seconds minutes)
  (* 60 minutes))

(define (hours->seconds hours)
  (* hours (minutes->seconds 60)))

(define (days->seconds days)
  (* days (hours->seconds 24)))

(define (in-time-interval? point left right)
  (and (>= point left) (<= point right)))

;;; functions to get atime, ctime, mtime from a fs-object
(define (fs-object-atime fs-object)
  (file-info:atime (fs-object-info fs-object)))

(define (fs-object-ctime fs-object)
  (file-info:ctime (fs-object-info fs-object)))

(define (fs-object-mtime fs-object)
  (file-info:mtime (fs-object-info fs-object)))

(define (fs-object-last-modified-in? fs-object pair)
  (in-time-interval? (fs-object-mtime fs-object) (car pair) (cdr pair)))

(define (fs-object-last-accessed-in? fs-object pair)
  (in-time-interval? (fs-object-atime fs-object) (car pair) (cdr pair)))

(define (fs-object-created-in? fs-object pair)
  (in-time-interval? (fs-object-ctime fs-object) (car pair) (cdr pair)))

(define (fs-object-name-matches? fso regexp)
  (regexp-search? regexp (fs-object-name fso)))

(define (ds-object-file-name-matches? fso regexp)
  (regexp-search? regexp (fs-object-file-name fso)))

;;; test stuff

;(dir-stream-display 
; (dir-stream-filter (dir-stream-from-dir-name "/Users/eric/tmp")
;		    (lambda (fs-object)
;		      (display (fs-object-mtime fs-object))
;		      (newline)
;		      (let ((one-week (days->seconds 7)))
;			(fs-object-created-in? fs-object 
;					       (cons (- (time) one-week)
;						     (time)))))
;		    (lambda (fs-object) #t)))
