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

;; FIXME
;; use lets for vectorrefs etc.
;; copy vector nodes into n-ary vectors (from median splitted vecs)

(define (make-b-tree-node l r)
  (let ((data #f)
        (left l)
        (right r))

    (define (get-data)
      data)

    (define (set-data! value)
      (set! data value))

    (define (set-left-with-index! i value)
      (cond ((not left)
             (display "not left")
             #f)
            (else (vector-set! left i value))))

    (define (set-right-with-index! i value)
      (cond ((not right)
             (display "not right")
             #f)
            (else (vector-set! right i value))))

    (define (get-left)
      left)

    (define (get-right)
      right)

    (define (dispatch msg)
      (lambda (msg)
        (cond ((eq? msg 'get-left)
               get-left)
              ((eq? msg 'get-right)
               get-right)
              ((eq? msg 'set-left-with-index!)
               set-left-with-index!)
              ((eq? msg 'set-right-with-index!)
               set-right-with-index!)
              ((eq? msg 'get-data)
               get-data)
              ((eq? msg 'set-data!)
               set-data!)
              (else (display "b-tree-node : message not understood")(newline)))))
    dispatch))

(define (make-b-tree n-ary);;NOTE FIXME n-ary and vector-length
  (let ((*tree (make-b-tree-node;; #f #f)))
                (make-vector n-ary (make-b-tree-node #f #f))
                (make-vector n-ary (make-b-tree-node #f #f)))))

    (define (vector-median v)
      (let ((len (ceiling (/ (vector-length v) 2))))
        (let ((retl (make-vector len (make-b-tree-node #f #f)))
              (retr (if (odd? len)
                        (make-vector (+ len 1) (make-b-tree-node #f #f))
                        (make-vector len (make-b-tree-node #f #f)))))
          (do ((i 0 (+ i 1)))
              ((= i len)(list retl retr))
            (vector-set! retl i (vector-ref v i))
            (vector-set! retr (- len (+ i 1)) (vector-ref v (- len (+ i 1))))
        ))))


    (define (search-rec str tree side-string) ;; root param in b-treenode
      (let* ((side-tree ((tree side-string)))
             (len (vector-length side-tree)))
        (do ((i 0 (+ i 1)))
            ((let* ((side-tree-el-first ((vector-ref side-tree i))))
               (cond ((>= i len 1);;last node
                      (do ((j 0 (+ j 1)))
                          ((= j len) 0)
                        (search-rec str (vector-ref j side-tree))))
                     ((let ((side-tree-el-second ((vector-ref side-tree (+ i 1)))))
                        (and (string<? str
                                       ((side-tree-el-first 'get-data)))
                             (string>? str
                                       ((side-tree-el-second 'get-data))))
                        (display "node not found in tree.") 0))
                     ((string=? str ((side-tree-el-first 'get-data)))
                      (display "string found in tree.") str)
                     (else (display "never reached."))))))))

    (define (search str)
      (search-rec str *tree 'get-left)
      (search-rec str *tree 'get-right))

    (define (dump-rec tree) ;; root param in b-treenode
      (if (not (tree 'get-left))
          0
          (let ((len (vector-length (tree 'get-left))))
            (do ((i 0 (+ i 1)))
            ((>= i len) 0)
            (display (((vector-ref (tree 'get-left) i)'get-data)))
            (dump-rec (vector-ref (tree 'get-left) i))
            )))
      (if (not (tree 'get-right))
          0
          (let ((len (vector-length (tree 'get-right))))
            (do ((i 0 (+ i 1)))
                ((>= i len) 0)
              (dump-rec (vector-ref (tree 'get-right) i))
              ))))

    (define (dump)
      (dump-rec *tree))

    (define (add-rec str tree) ;; root param in b-treenode ;; refactor call-with-values

      (let ((lefttree (tree 'get-left));;FIXME ()
            (righttree (tree 'get-right)))
               ;;len (vector-length ((tree 'get-left)))))
        ;;(call-with-values
        ;;    (lambda () (values lefttree righttree))
        ;;  (lambda (lefttree righttree)
            (add-rec-side-tree str lefttree)
            (add-rec-side-tree str righttree)
        ;;    ))
            ))

    (define (add-rec-side-tree str side-tree)
      (do ((i 0 (+ i 1)))
          ((cond ((not side-tree)
                  #f)
                 ((let* ((data (((vector-ref side-tree i) 'get-data)))
                         (left-and-right (vector-median side-tree));;FIXME right also descend
                         (new-node (make-b-tree-node
                                    (car left-and-right)
                                    (cadr left-and-right))));;FIXME lenght mustbe n-ary

                    (or data (and (string? data)(string=? data "")))
                    ((new-node 'set-data!) str)
                    ;;((side-tree 'set-left-with-index!) i new-node)
                    (vector-set! side-tree i new-node)
                    (set! i (vector-length side-tree))))

                 ((let ((data (((vector-ref side-tree i) 'get-data))))
                    (or ;;(and (string=?)
                        ;;     (string>? str data))
                        (and (string? data)
                             (string=? data ""))
                        (and (string? data)
                             (string<? str data)
                             (string>? str data)))
                  (let ((left-and-right (vector-median side-tree)))
                    (let ((new-node (make-b-tree-node (car left-and-right) (cadr left-and-right))))
                      ((new-node 'set-data!) str)
                      (vector-set! side-tree i new-node)
                      ))))
                 ;;((= i len)
                 ;; (display "node not added .") 0)

                 (else (display "b-tree add - never reached.")))))
      (do ((i 0 (+ i 1)))
          ((= i (vector-length side-tree))0)
        (let ((side-tree-node (vector-ref i side-tree)))
          (cond ((not (not side-tree-node))
                 #f)
                (else (add-rec str side-tree-node)))));;NOTE add-rec not the other add-rec
      )



    (define (add str)
      (add-rec str *tree)
      )

    (define (dispatch msg)
      (cond ((eq? msg 'add) add)
            ((eq? msg 'search) search)
            ((eq? msg 'dump) dump)
            (else (display "b-tree : message not understood.")(newline))))
    dispatch))

;; test program
(define bt (make-b-tree 2))
((bt 'add)"abc")
;;((bt 'add)"def")
;;((bt 'add)"hij")
;;((bt 'search)"abc")
;;((bt 'dump))
