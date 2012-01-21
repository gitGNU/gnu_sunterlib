;;; b-tree.scm - a B-tree for Xanadu
;;;
;;; Copyright (c) 2012 Johan Ceuppens
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

(define (make-b-tree-node l r)
  (let ((numitems 0)
        (numnodes 0)
        (root 'foo)
        (data #f)
        (left l)
        (right r))

    (define (get-data)
      data)

    (define (set-data! value)
      (set! data value))

    (define (get-left)
      (display "get-left")
      left)

    (define (get-right)
      right)

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
              ((eq? msg 'get-data)
               get-data)
              ((eq? msg 'set-data!)
               set-data!)
              ((eq? msg 'get-right)
               get-right)
              (else (display "b-tree-node : message not understood")))))
    dispatch))

(define (make-b-tree n-ary);;NOTE FIXME n-ary and vector-length
  (let ((*tree (make-b-tree-node;; #f #f)))
                (make-vector n-ary (make-b-tree-node #f #f))
                (make-vector n-ary (make-b-tree-node #f #f)))))

    (define (vector-median v)
      (let ((len (ceiling (/ (vector-length v) 2))))
        (let ((retl (make-vector len))
              (retr (if (odd? len) (make-vector (- len 1)) (make-vector len))))
          (do ((i 0 (+ i 1)))
              ((= i len) (list retl retr))
            (vector-set! retl i (vector-ref v i))
	(vector-set! retr (- len (- i 1)) (vector-ref v (- len (- i 1))))
        ))))


    (define (search-rec str tree) ;; root param in b-treenode
      (let ((len (vector-length tree)))
        (do ((i 0 (+ i 1)))
            ((cond ((>= i (- len 1));;last node
                    (do ((j 0 (+ j 1)))
                        ((= j len) (display "node not found.") 0)
                      (search-rec str (vector-ref j tree))))
                   ((and (string<? str (((vector-ref tree i)'get-data)))
                         (string>? str (((vector-ref tree (+ i 1))'get-data))))
                    (display "node not found in tree.") 0)
                   ((string=? str (((vector-ref tree i)'get-data)))
                    (display "string found in tree.") str)
                   (else (display "never reached."))))
          )))

    (define (search str)
      (search-rec str *tree))

    (define (dump-rec tree) ;; root param in b-treenode
      (if (not (vector? tree))
          0
          (let ((len (vector-length tree)))
            (do ((i 0 (+ i 1)))
                ((>= i len) 0)
              (display 'foo)
              (display (((vector-ref tree i)'get-data)))
              (display 'foo)
              (dump-rec (vector-ref tree i))
              ))))

    (define (dump)
      (dump-rec *tree))

    (define (add-rec str tree) ;; root param in b-treenode ;; refactor call-with-values
      (let ((lefttree (tree 'get-left));;FIXME
            (righttree (tree 'get-right)))
               ;;len (vector-length ((tree 'get-left)))))
        (call-with-values
            (lambda () (values lefttree righttree))
          (lambda (lefttree righttree)
            (do ((i 0 (+ i 1)))
                ((cond ((or lefttree
                            (not (((vector-ref lefttree i) 'get-data)))
                            (string=? (((vector-ref lefttree i) 'get-data)) ""))
                        (display 'FOO1)
                        (let ((lr (vector-median lefttree)))
                          (let ((new-node (make-b-tree-node (car lr) (cadr lr))));;FIXME lenght mustbe n-ary
                            ((new-node 'set-data!) str)
                            (vector-set! lefttree i new-node)
                            )))
                       ((and (string<? str (((vector-ref lefttree i) 'get-data)))
                             (string>? str (((vector-ref lefttree (+ i 1)) 'get-data))))
                        (let ((lr (vector-median lefttree)))
                          (let ((new-node (make-b-tree-node (car lr) (cadr lr))))
                            ((new-node 'set-data!) str)
                            (vector-set! lefttree i new-node)
                            )))
                       ;;((string=? str (((vector-ref lefttree i)'get-data)))
                       ;; (set! i (vector-length tree)))
                       ((>= i (- len 1));;last node
                        (do ((j 0 (+ j 1)))
                            ((= j len) (display "node not added.") 0)
                          (add-rec str (vector-ref lefttree i)))
                        )

                       (else (display "never reached.")))))
            ))))

    (define (add str)
      (display "FOO")
      (add-rec str *tree)
      (display "FOO")
      )

    (define (dispatch msg)
      (cond ((eq? msg 'add) add)
            ((eq? msg 'search) search)
            ((eq? msg 'dump) dump)
            (else (display "b-tree : message not understood."))))
    dispatch))


(define bt (make-b-tree 2))
((bt 'add)"abc")
;;((bt 'dump))
