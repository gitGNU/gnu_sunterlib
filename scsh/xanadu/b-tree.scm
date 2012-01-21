;;; b-tree.scm - a B-tree for Xanadu
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

(define (make-b-tree-node left right)
  (let ((numitems 0)
        (numnodes 0)
        (root 'foo)
        (left left)
        (right right))

    (define (get-root)
      root)

    (define (get-numitems)
      numitems)

    (define (get-numnodes)
      numnodes)

    (define (dispatch msg)
      (lambda (msg)
        (cond ((eq? msg 'get-root)
               get-root)
              ((eq? msg 'get-numitems)
               get-numitems)
              ((eq? msg 'get-numnodes)
               get-numnodes)
              ((eq? msg 'get-left)
               get-left)
              ((eq? msg 'get-right)
               get-right)
              (else (display "b-tree-node : message not understood")))))
     dispatch)) 

(define (make-b-tree)
  (let ((*tree (make-vector 0)))

    (define (vector-median v)
      (let ((len (ceiling (/ (vector-length v) 2))))
        (let ((retl (make-vector len))
              (retr (if (odd? len) (make-vector (- len 1)) (make-vector len))))
          (do ((i 0 (+ i 1)))
              ((= i len) (list retl retr))
            (vector-set! retl i (vector-ref v i))
	(vector-set! retr (- len (- i 1)) (vector-ref v (- len (- i 1))))
        ))))

    (define (goto-left-node str i tree)
      (cond ((= (vector-length tree) i) (display "null node") (vector-set! tree i str))
            (else #f)))

    (define (grow-up upper-node)
             )

    (define (add-rec str tree upper-node)
      (let ((len (vector-length tree)))
        (do ((i 0 (+ i 1)))
            ((cond ((>= i (- len 1));;last node
                    (grow-up upper-node))
                   ((and (string<? str (vector-ref tree i))
                         (string>? str (vector-ref tree (+ i 1))))
                    (let ((new-node (make-b-tree-node l r))
                          (lr (vector-median tree)))
                      (vector-set! tree i new-node (car lr)(cadr lr))) 
                      ;;(grow-down (vector-ref tree i)
                      )
                   ((string>=? str (vector-ref tree i))
                    (grow-down))
                   ((string=? str (vector-ref tree i))
                    (set! i (vector-length tree)))
                   (else (display "never reached."))))
          )))


    (define (add str)
      (add-rec *tree *tree))

    (define (dispatch msg)
      (cond ((eq? msg 'add) add)
            (else (display "b-tree : message not understood."))))
	dispatch))
