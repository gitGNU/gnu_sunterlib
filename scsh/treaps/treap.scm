(program 
 (requires srfi-9 srfi-23 srfi-27)
(code
;;; This file and the accompanying README were derived from
;;; Oleg's code for Gambit available from
;;;
;;;        http://okmij.org/ftp/Scheme/lib/treap.scm
;;;

(define-record-type treap
  (really-make-treap key-compare size root)
  treap?
  (key-compare treap-key-compare)
  (size treap-size set-treap-size!)
  (root treap-root set-treap-root!))

(define (make-treap key-compare)
  (really-make-treap key-compare 0 #f))

;; a node of a tree, a record of
;;   key, anything that key-compare could be applied to
;;   value, any object associated with the key
;;   left-kid, #f if absent
;;   right-kid
;;   prio, a priority of the node (a FIXNUM random number)


(define-record-type node
  (make-node key value left-kid right-kid priority)
  node?
  (key node:key)
  (value node:value node:value-set!)
  (left-kid node:left-kid node:left-kid-set!)
  (right-kid node:right-kid node:right-kid-set!)
  (priority node:priority))

(define random
  (let ((max (- (expt 2 15) 1)))
    (lambda ()
      (random-integer max))))

(define (new-leaf key value)
  (make-node key value #f #f (random)))

(define (node:key-value node)
  (cons (node:key node)
        (node:value node)))

(define (node:unsubordination? parent kid)
  (> (node:priority parent) (node:priority kid)))

(define-syntax node:dispatch-on-key
  (syntax-rules ()
    ((node:dispatch-on-key treap node key on-less on-equal on-greater)
     (let ((result ((treap-key-compare treap) key (node:key node))))
       (cond
        ((zero? result) on-equal)
        ((positive? result) on-greater)
        (else on-less))))))

(define (n-display . args)
  (for-each display args))

(define (node:debugprint node)
  (n-display " " (node:key-value node) ", kids "
             (cons (not (not (node:left-kid node)))
                   (not (not (node:right-kid node))))
             ", prio " (node:priority node) #\newline))




;; Looking up assocaitions in a treap: just like in any search tree
;; Given  a key, return the corresponding (key . value) association
;; in the treap, or #f if the treap does not contain an association
;; with that key
;; This procedure takes as many comparisons (evaluations of the
;; key-compare procedure) as the depth of the found node
(define (locate-assoc treap key)
  (let loop ((node (treap-root treap)))
    (and node
         (node:dispatch-on-key treap node key
                               (loop (node:left-kid node))
                               (node:key-value node)
                               (loop (node:right-kid node))))))

(define (locate-extremum-node treap branch-selector)
  (let ((root (treap-root treap)))
    (if (not root) (error "empty tree")
        (let loop ((node root) (parent #f))
          (if node (loop (branch-selector node) node)
              (node:key-value parent))))))

                                        ; in-order traversal of the treap
(define (for-each-inorder treap primary-branch-selector secondary-branch-selector)
  (let ((root (treap-root treap)))
    (lambda (proc)
      (if (not root) (error "empty tree")
          (let loop ((node root))
            (if node
                (begin
                  (loop (primary-branch-selector node))
                  (proc (node:key-value node))
                  (loop (secondary-branch-selector node)))))))))

(define (get-depth treap)
  (let ((root (treap-root treap)))
    (let loop ((node root) (level 0))
      (if (not node) level
          (max (loop (node:left-kid node) (+ 1 level))
               (loop (node:right-kid node) (+ 1 level)))))))

;; debug printing of all nodes of the tree in-order
;; in an ascending order of keys
(define (debugprint treap)
  (let ((root (treap-root treap)))
    (n-display  #\newline
                "The treap contains " (treap-size treap) " nodes"
                #\newline)
    (let loop ((node root) (level 0))
      (if node
          (begin
            (loop (node:left-kid node) (+ 1 level))
            (n-display "  level " level)
            (node:debugprint node)
            (loop (node:right-kid node) (+ 1 level))))
      (newline))))

;; Adding a new association to the treap (or replacing the old one
;; if existed). Return the (key . value) pair of an old (existed
;; and replaced association), or #f if a new association was really
;; added
(define (insert! treap key value)
  (let ((root (treap-root treap)))
    (letrec ((new-node (new-leaf key value))
             (old-key-value #f)
             ;; If the left branch of parent is empty, insert the
             ;; new node there, check priorities
             ;; Otherwise, descend recursively
             ;; If the parent got inverted due to a right rotation,
             ;; return the new parent of the branch; otherwise,
             ;; return #f (indicating no further checks are necessary)
             (insert-into-left-branch
              (lambda (key parent)
                (let ((old-left-kid (node:left-kid parent)))
                  ;; Found a place to insert the 'new-node': as the left
                  ;; leaf of the parent
                  (if (not old-left-kid)
                      (cond
                       ((node:unsubordination? parent new-node)
                        ;; Right rotation over the new-leaf
                        (node:right-kid-set! new-node parent)
                        new-node)	;; becomes a new parent
                       (else
                        (node:left-kid-set! parent new-node)
                        #f))
                      ;; Insert the new-leaf into a branch rooted
                      ;; on old-left-kid
                      (let ((new-left-kid
                             (node:dispatch-on-key treap old-left-kid key
                                                   (insert-into-left-branch key old-left-kid)
                                                   (update-existing-node old-left-kid)
                                                   (insert-into-right-branch key old-left-kid))))
                        (and new-left-kid
                             ;; That branch got a new root
                             (cond
                              ((node:unsubordination? parent new-left-kid)
                               ;; Right rotation over the new-left-kid
                               (node:left-kid-set! parent
                                                   (node:right-kid new-left-kid))
                               (node:right-kid-set! new-left-kid parent)
                               new-left-kid) ;; becomes a new parent
                              (else
                               (node:left-kid-set! parent new-left-kid)
                               #f))))
                      ))))

             ;; If the right branch of parent is empty, insert the
             ;; new node there, check priorities
                                        ; Otherwise, descend recursively
             ;; If the parent got inverted due to a left rotation,
             ;; return the new parent of the branch; otherwise,
             ;; return #f (indicating no further checks are necessary)
             (insert-into-right-branch
              (lambda (key parent)
                (let ((old-right-kid (node:right-kid parent)))
                  ;; Found a place to insert the 'new-node': as the right
                  ;; leaf of the parent
                  (if (not old-right-kid)
                      (cond
                       ((node:unsubordination? parent new-node)
                        ;; Left rotation over the new-leaf
                        (node:left-kid-set! new-node parent)
                        new-node)	; becomes a new parent
                       (else
                        (node:right-kid-set! parent new-node)
                        #f))
                      ;; Insert the new-leaf into a branch rooted
                      ;; on old-right-kid
                      (let ((new-right-kid
                             (node:dispatch-on-key treap old-right-kid key
                                                   (insert-into-left-branch key old-right-kid)
                                                   (update-existing-node old-right-kid)
                                                   (insert-into-right-branch key old-right-kid))))
                        (and new-right-kid
                             ;; That branch got a new root
                             (cond
                              ((node:unsubordination? parent new-right-kid)
                               ;; Left rotation over the new-right-kid
                               (node:right-kid-set! parent
                                                    (node:left-kid new-right-kid))
                               (node:left-kid-set! new-right-kid parent)
                               new-right-kid) ; becomes a new parent
                              (else
                               (node:right-kid-set! parent new-right-kid)
                               #f))))
                      ))))

             (update-existing-node
              (lambda (node)
                (set! old-key-value (node:key-value node))
                (node:value-set! node value)
                #f))
             )                          ; end of letrec

      ;; insert's body
      (cond
       ;; insert into an empty tree
       ((not root) (set-treap-root! treap new-node))

       (else
        (let ((new-root
               (node:dispatch-on-key treap root key
                                     (insert-into-left-branch key root)
                                     (update-existing-node root)
                                     (insert-into-right-branch key root))))
          (if new-root
              (set-treap-root! treap  new-root)))))
      (if (not old-key-value)
          (set-treap-size! treap (+ (treap-size treap) 1))) ; if the insertion has really occurred
      old-key-value)))


;; Deleting existing associations from the treap

(define (delete-extremum-node! treap branch-selector
                               branch-setter the-other-branch-selector)
  (let ((root (treap-root treap)))
    (cond
     ((not root) (error "empty tree"))
     ((not (branch-selector root))	; root is the extreme node
      (let ((result (node:key-value root)))
        (set-treap-root! treap (the-other-branch-selector root))
        (set-treap-size! treap (- (treap-size treap) 1))
        result))
     (else
      (let loop ((node (branch-selector root)) (parent root))
        (let ((kid (branch-selector node)))
          (if kid (loop kid node)
              (let ((result (node:key-value node)))
                (branch-setter parent (the-other-branch-selector node))
                (set-treap-size! treap (- (treap-size treap) 1))
                result))))))))

;; Given two treap branches (both of which could be empty)
;; which satisfy both the order invariant and the priority invariant
;; (all keys of all the nodes in the right branch are strictly bigger
;; than the keys of left branch nodes), join them
;; while keeping the sorted and priority orders intact
(define (join! treap left-branch right-branch)
  (cond
   ((not left-branch) right-branch)	; left-branch was empty
   ((not right-branch) left-branch)	; right-branch was empty
   ((node:unsubordination? left-branch right-branch)
    ;; the root of the right-branch should be the new root
    (node:left-kid-set! right-branch
                        (join! treap left-branch (node:left-kid right-branch)))
    right-branch)
   (else
    ;; the root of the left-branch should be the new root
    (node:right-kid-set! left-branch
                         (join! treap (node:right-kid left-branch) right-branch))
    left-branch)))


;; Find an association with a given KEY, and delete it.
;; Return the (key . value) pair of the deleted association, or
;; #f if it couldn't be found
(define (delete! treap key)
  (define (delete-node! node parent from-left?)
    (let ((old-assoc (node:key-value node))
          (new-kid (join! treap (node:left-kid node) (node:right-kid node))))
      (set-treap-size! treap (- (treap-size treap) 1))
      (if parent
          (if from-left?
              (node:left-kid-set! parent new-kid)
              (node:right-kid-set! parent new-kid))
          ;; Deleting of the root node
          (set-treap-root! treap new-kid))
      old-assoc))

  (let loop ((node (treap-root treap)) (parent #f) (from-left? #t))
    (and node
         (node:dispatch-on-key treap node key
                               (loop (node:left-kid node) node #t)
                               (delete-node! node parent from-left?)
                               (loop (node:right-kid node) node #f)))))

(define (apply-default-clause key default-clause)
  (cond
   ((null? default-clause)
    (error "key " key " was not found in the treap "))
   ((pair? (cdr default-clause))
    (error "default argument must be a single clause"))
   ((procedure? (car default-clause)) ((car default-clause)))
   (else (car default-clause))))

(define (treap-get treap key . default-clause)
  (or (locate-assoc treap key) (apply-default-clause key default-clause)))

(define (treap-delete! treap key . default-clause)
  (or (delete! treap key) (apply-default-clause key default-clause)))

(define (treap-get-min treap)
  (locate-extremum-node treap node:left-kid))

(define (treap-get-max treap)
  (locate-extremum-node treap node:right-kid))

(define (treap-delete-min! treap)
  (delete-extremum-node! treap
                         node:left-kid node:left-kid-set!
                         node:right-kid))

(define (treap-delete-max! treap)
  (delete-extremum-node! treap
                         node:right-kid node:right-kid-set!
                         node:left-kid))

(define (treap-empty? treap)
  (not (treap-root treap)))

(define (treap-depth treap)
  (get-depth treap))

(define (treap-clear! treap)
  (set-treap-root! treap #f)
  (set-treap-size! treap 0))

(define treap-put! insert!)

(define (treap-for-each-ascending treap proc)
  ((for-each-inorder treap node:left-kid node:right-kid) proc))

(define (treap-for-each-descending treap proc)
  ((for-each-inorder treap node:right-kid node:left-kid) proc))

(define treap-debugprint debugprint)
))