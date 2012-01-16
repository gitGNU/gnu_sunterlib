;;; tree.scm - twentyseven tree, extended octree
;;;
;;; Copyright (c) 2011-2012 Johan Ceuppens
;;;
;;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define (aspecterror)
  (display "message not understood"))

(define (make-leaf x y z w h depth)
  (let ((*collide 0))

    (define (get-x)
      x)

    (define (get-y)
      y)

    (define (get-z)
      z)

    (define (get-w)
      w)

    (define (get-h)
      h)

    (define (get-d)
      depth)


    (define (set-collide value)
      (set! *collide value))

    (lambda (msg)
      (cond ((eq? msg 'x)get-x)
	    ((eq? msg 'y)get-y)
	    ((eq? msg 'z)get-z)
	    ((eq? msg 'w)get-w)
	    ((eq? msg 'h)get-h)
	    ((eq? msg 'depth)get-d)
	    ((eq? msg 'isleaf)#t)
	    ((eq? msg 'set-collide)set-collide)
	    (else (display "leaf : ")(aspecterror))
	    ))))


(define (make-node x y z w h depth)
  (let ((*leafcode (make-leaf x y z w h depth))
	(*nodes '()))

    (define (add n)
      (set! *nodes (append *nodes (list n))))

    ;; get node with index
    (define (get idx)
      (list-ref *nodes idx))

    (lambda (msg)
      (cond
       ((eq? msg 'isleaf) #f)
       ((eq? msg 'get) get)
       ((eq? msg 'add) add)
       (else (*leafcode msg))
       ))
    ))

(define (make-twentyseventree x y z w h depth)
  (let ((*tree (make-node x y z w h depth)))

    (define (generate! border)
      (generaterec *tree x y z w h depth border))

    (define (generaterec node x y z w h d border)
      (if (and ;;(procedure? node)
	   (<= d 0))
	  (begin
	    (do ((i 0 (+ i 1)))
		((< i 4)#t)
	      ((node 'add)(make-leaf (/ (* i x)4)  (/ y 4)    (/ z 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
	    (do ((i 0 (+ i 1)))
		((< i 4)#t)
	      ((node 'add)(make-leaf (/ (* i x)4)  (/ (* i y) 4)    (/ z 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
	    (do ((i 0 (+ i 1)))
		((< i 4)#t)
	      ((node 'add)(make-leaf (/ (* i x)4)  (/ (* i y) 4)    (/ (*i z) 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
	    (do ((i 0 (+ i 1)))
		((< i 4)#t)
	      ((node 'add)(make-leaf (/ x 4)  (/ (* i y) 4)    (/ z 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
	    (do ((i 0 (+ i 1)))
		((< i 4)#t)
	      ((node 'add)(make-leaf (/ x 4)  (/ (* i y) 4)    (/ (* i z) 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
	    (do ((i 0 (+ i 1)))
		((< i 4)#t)
	      ((node 'add)(make-leaf (/ (* i x)4)  (/ y 4)    (/ (*i z) 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
	    (do ((i 0 (+ i 1)))
		((< i 4)#t)
	      ((node 'add)(make-leaf (/ x 4)  (/ y 4)    (/ (*i z) 4)   (/ w 3) (/ h 3) (/ d 3) depth)))

	    ;; center node
	    (do ((i 0 (+ i 1)))
		((< i 21)#t)
	      (let ((tempn ((node 'get) i)))
		(if (< border (+ ((tempn 'x) ((tempn 'w))))
		       ;; borders set for depth see
		       (set! border (+ border 	border)))
		    ((((node 'get) i)'set-collide) 1)
		    )))
	    );;begin
	  (if (> depth 0)
	      (begin

		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  ((node 'add)(make-node (/ (* i x)4)  (/ y 4)    (/ z 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  ((node 'add)(make-node (/ (* i x)4)  (/ (* i y) 4)    (/ z 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  ((node 'add)(make-node (/ (* i x)4)  (/ (* i y) 4)    (/ (*i z) 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  ((node 'add)(make-node (/ x 4)  (/ (* i y) 4)    (/ z 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  ((node 'add)(make-node (/ x 4)  (/ (* i y) 4)    (/ (* i z) 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  ((node 'add)(make-node (/ (* i x)4)  (/ y 4)    (/ (*i z) 4)   (/ w 3) (/ h 3) (/ d 3) depth)))
		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  ((node 'add)(make-node (/ x 4)  (/ y 4)    (/ (*i z) 4)   (/ w 3) (/ h 3) (/ d 3) depth)))

		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  (generaterec (- depth 1) ((node 'get)i) (* i (/ x 4)) (/ y 4) (/ z 4) (/ w 3) (/ h 3) (/ d 3) border))
		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  (generaterec (- depth 1) ((node 'get)(+ i 2))  (/ (* i x) 4) (/ (* i y) 4) (/ z 4) (/ w 3) (/ h 3) (/ d 3) border))
		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  (generaterec (- depth 1) ((node 'get)(+ i 5)) (/ (* i x) 4) (/ (* i y) 4) (/ (* i z) 4) (/ w 3) (/ h 3) (/ d 3) border))
		(do ((i 0 (+ i 1)))
		      ((< i 4)#t)
		  (generaterec (- depth 1) ((node 'get)(+ i 8)) (/ x 4) (/(* i y) 4) (/ z 4) (/ w 3) (/ h 3) (/ d 3) border))
		(do ((i 0 (+ i 1)))
		      ((< i 4)#t)
		  (generaterec (- depth 1) ((node 'get)(+ i 11)) (/ x 4) (/(* i y) 4) (/ z 4) (/ w 3) (/ h 3) (/ d 3) border))
		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  (generaterec (- depth 1) ((node 'get)(+ i 14)) (/ (* i x) 4) (/ y 4) (/ (* i z) 4) (/ w 3) (/ h 3) (/ d 3) border))
		(do ((i 0 (+ i 1)))
		    ((< i 4)#t)
		  (generaterec (- depth 1) ((node 'get)(+ i 17)) (/ x 4) (/ y 4) (/ (* i z) 4) (/ w 3) (/ h 3) (/ d 3) border))
		  )))
	    )
    ;; px = Player's x coordinate in the treeworld,
    ;; pdepth = Player's depth in the world tree map
    (define (collision depth px py pz pdepth)
      (collisionrec depth *tree px py pz pdepth)
      )

    (define (collisionrec depth node px py pz pdepth)
      (if (and ;;(procedure? node)
	       (< px (+ ((node 'x)) (* ((node 'w)) (+ 1 (- depth)))))
	       (> (+ px ((node 'w))) ((node 'x)))
	       (<  py (+ ((node 'y)) (* ((node 'h))) (+ (- depth) 1)))
	       (> y ((node 'y)))
	       )

	  (cond ((<= depth pdepth)
		 1)
		((< pdepth depth)
		 2)
		((> pdepth depth)
		 3)
		(else
		 (begin
		   (do ((i 0 (+ i 1)))
		       (< i 21)
		     (let ((n ((node 'get)i)))
		       (if (and n (not ((n 'isleaf))
				       ))
			   (collisionrec (- depth 1) n px py pz pdepth))))
		   )))))


    (lambda (msg)
      (cond
       ((eq? msg 'generate!)generate!)
       ((eq? msg 'collide) collision)
       (else (display "twentyseventree : ")(aspecterror)))
      )))
