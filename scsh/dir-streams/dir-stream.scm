;;; directories represented as streams

;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; Copyright (c) 2002 by Eric Knauel
;;; Copyright (c) 2002 by Matthias Neubauer
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;; dir-stream ===
;; (make-dir-stream file-info (stream of file-info) (stream of dir-stream)

(define-record-type dir-stream :dir-stream
  (make-dir-stream dir-info files-stream subdir-stream)
  dir-stream?
  (dir-info dir-stream-info)
  (files-stream dir-stream-files-stream)
  (subdir-stream dir-stream-subdir-stream))

(define-record-type fs-object :fs-object
  (really-make-fs-object parent name info)
  fs-object?
  (parent fs-object-parent)
  (name fs-object-name)
  (info fs-object-info))

(define-record-discloser :fs-object
  (lambda (r)
    `(fs-object ,(fs-object-name r) ,(fs-object-parent r))))

(define (make-fs-object parent name chase?)
  (really-make-fs-object
   parent
   name
   (file-info (combine-path parent name) chase?)))

(define (combine-path parent name)
  (if (string=? parent "")
      name
      (string-append parent
		     "/"
		     name)))

(define (fs-object-filename fs-object)
  (combine-path (fs-object-parent fs-object)
		(fs-object-name fs-object)))


(define (dir-stream-from-dirname dirname . args)
  ;; skip file in case of an error during file-info
  (define (next-info ds parent dirname chase?)
    (let ((file (read-directory-stream ds))) 
      (if file
	  (call-with-current-continuation
	   (lambda (k)
	     (with-handler
	      (lambda (cond more)
		(k (next-info ds parent dirname chase?)))
	      (lambda ()
		(cons (make-fs-object (combine-path parent dirname)
				      file
				      chase?)
		      ds)))))
	  (begin (close-directory-stream ds) #f))))
  (let-optionals args ((chase? #t) (parent ""))
    (let ((info-stream (stream-unfold
			(lambda (ds)
			  (next-info ds parent dirname chase?))
			(call-with-current-continuation
			 (lambda (k)
			   (with-handler
			    (lambda (cond more)
			      (make-empty-stream))
			    (lambda ()
			      (open-directory-stream (combine-path parent dirname)))))))))
      (make-dir-stream
       (make-fs-object parent dirname chase?)
       (stream-filter-map
	(lambda (fso) (and (not (file-info-dir? (fs-object-info fso)))
			   fso))
	info-stream)
       (stream-filter-map
	(lambda (fso)
	  (and (file-info-dir? (fs-object-info fso))
	       (dir-stream-from-dirname
		(fs-object-name fso) chase? (fs-object-parent fso))))
	info-stream)))))

(define (dir-stream-filter ds file-pred dir-pred)
  (make-dir-stream (dir-stream-info ds)
		   (stream-filter file-pred
				  (dir-stream-files-stream ds))
		   (stream-filter-map
		    (lambda (subdir)
		      (and (dir-pred (dir-stream-info subdir))
			   (dir-stream-filter subdir file-pred dir-pred)))
		    (dir-stream-subdir-stream ds))))

;; dir-stream a b = make-dir-tream a (stream of b) (stream of (dir-stream a b))
;; dir-stream-fold-right: dir-stream a b -> (a -> c -> d -> e) ->  
;;                        (b -> c -> c) -> c ->
;;                        (e -> d -> d) ->  d -> e
;; Krass Sach!!!

(define (dir-stream-fold-right ds make-dir-stream
			       files-make-stream files-stream-empty 
			       subdirs-make-stream subdirs-empty)
  (make-dir-stream
   (dir-stream-info ds)
   (stream-fold-right files-make-stream files-stream-empty (dir-stream-files-stream ds))
   (stream-fold-right (lambda (subdir accu)
			(subdirs-make-stream
			 (dir-stream-fold-right subdir make-dir-stream files-make-stream 
						files-stream-empty subdirs-make-stream subdirs-empty)
			 accu))
		      subdirs-empty
		      (dir-stream-subdir-stream ds))))

; Example:
; (define (disc-usage ds)
;   (dir-stream-fold-right ds (lambda (fso sum subdirs) (list (fs-object-filename fso)
; 							    (apply + sum (map cadr subdirs))
; 							    subdirs))
; 			 (lambda (fso accu)
; 			   (+ accu (file-info:size (fs-object-info fso))))
; 			 0
; 			 cons
; 			 '()))

(define (apply-to-dir-stream stream-f)
  (define (f ds file-f dir-f)
    (make-dir-stream
     (dir-f (dir-stream-info ds))
     (stream-f file-f (dir-stream-files-stream ds))
     (stream-f (lambda (sub-ds)
		   (f sub-ds file-f dir-f))
	       (dir-stream-subdir-stream ds))))
  f)

(define dir-stream-map (apply-to-dir-stream stream-map))
(define dir-stream-filter-map (apply-to-dir-stream stream-filter-map))

(define (dir-stream-for-each ds file-f dir-f)
  (dir-f (dir-stream-info ds))
  (stream-for-each file-f (dir-stream-files-stream ds))
  (stream-for-each 
   (lambda (sub-ds)
     (dir-stream-for-each sub-ds file-f dir-f))
   (dir-stream-subdir-stream ds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file-info-dir? fi)
  (eq? (file-info:type fi) 'directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;(filter x file? dir?) -> x 

;(define (filter p l)
;  (fold-right (lambda (e a)
;		(if (p e)
;		    (cons e a)
;		    a))
;	      '()
;	      l))
;(define-structure dir :dir
;  (make-dir info files))
  
;; dir = (make-dir dir-info (stream of (union file-info dir)))

;(define (dir-filter dir file-pred dir-pred) ; dir -> p1 -> p2 -> dir
;  (make-dir
;   (dir-info dir)
;   (let lp ((stream (dir-files dir)))	; stream -> steam
;     (if (stream-empty? stream)
;	 (make-empty-stream)
;	 (let ((head (stream-head dir)))
;	   (cond ((file? head)
;		  (if (file-pred head)
;		      (make-stream 
;		       (lambda () head)
;		       (lambda () (lp (stream-tail stream))))
;		      (lp (stream-tail stream))))
;		 (else (if (dir-pred (dir-info head))
;			   (make-steam 
;			    (lambda () (dir-filter head file-pred dir-pred))
;			    (lambda () (lp (steam-tail stream))))
;			   (lp (stream-tail stream))))))))))

  
