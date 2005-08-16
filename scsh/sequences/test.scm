;; poor man's testsuite for sequence-lib (fairly incomplete)
;; Run stuff  ,in sequence-testbed


(define (true/1 x) #t)
(define (false/1 x) #f)

(define (for-empty-seqs proc)
  (for-each proc '(#() "" ())))

;; SEQUENCE-EVERY on various types of (empty) seqs
(for-empty-seqs (lambda (s)
                  (assert (sequence-every number? s))))

;; basic SEQUENCE-EVERY
(assert (sequence-every number? '(1 2 3)))
(deny (sequence-every number? '(1 six)))

;; SEQUENCE-EVERY with start arg
(assert (sequence-every number? '#(x 4) 1))
(deny (sequence-every number? '#(4 x 4) 1))

;; SEQUENCE-EVERY with start/end args
(assert (sequence-every number? '#(a b 0.1 d e) 2 2))
(assert (sequence-every number? '#(a b 0.1 d e) 2 3))
(deny (sequence-every number? '#(a b 0.1 d e) 2 4))

;; SEQUENCES-EVERY on various types of (empty) seqs
(for-empty-seqs (lambda (s)
                  (assert (sequences-every number? s))))

;; SEQUENCES-EVERY on one seq
(assert (sequences-every number? '#(1 2 3)))
(deny (sequence-every number? '(1 six)))

;; SEQUENCES-EVERY on two seqs
(assert (sequences-every = '#() '()))
(assert (sequences-every char=? '(#\a #\b) "abc"))
(deny (sequences-every eqv? '(1 2) '#(1 six)))

;; SEQUENCE-ANY on various types of (empty) lists
(for-empty-seqs (lambda (s)
                  (deny (sequence-any number? s))))

;; basic SEQUENCE-ANY
(assert (sequence-any number? '#(a 6 c)))
(deny (sequence-any number? '#(a b c)))

;; SEQUENCE-ANY with start arg
(assert (sequence-any number? '#(x 4) 1))
(deny (sequence-any number? '#(4 x) 1))

;; SEQUENCE-ANY with start/end args
(deny (sequence-any number? '(1 2 3) 2 2))
(assert (sequence-any number? '#(a b 0.1 d e) 2 3))
(deny (sequence-any number? '(x y)))

;; SEQUENCES-ANY on various types of (empty) sequences
(for-empty-seqs (lambda (s)
                  (deny (sequences-any number? s))))

;; SEQUENCES-ANY on one seq
(assert (sequences-any number? '#(a 6 c)))
(deny (sequences-any number? '#(a b c)))

;; SEQUENCES-ANY on two seqs
(deny (sequences-any = '#() '()))
(assert (sequences-any char=? '(#\a #\x) "abc"))
(deny (sequences-any eqv? '(2 1) '#(1 2)))

(assert (equal? (sequence->list "afto" 2)
                '(#\t #\o)))
(assert (equal? (sequence->list "afto" 2 2)
                '()))

(assert (equal? (sequence-copy/maker make-vector "aeiou zeta" 3)
                '#(#\o #\u #\space #\z #\e #\t #\a)))

(assert (string=? (sequence-copy "aeiou zeta" 3)
                  "ou zeta"))

(let ((s (make-vector 6 'a)))
  (sequence-copy! s 1 '(1 2 3) 1 3)
  (assert (equal? s '#(a 2 3 a a a))))
(let ((s (make-string 6 #\a)))
  (sequence-copy! s 1 "gNOblok" 1 3)
  (assert (equal? s "aNOaaa")))

;; SEQUENCE-COPY! from sequence into itself
(let ((s (vector 0 1 2 3 4 5 6)))
  (sequence-copy! s 1 s 3 6)
  (assert (equal? s '#(0 3 4 5 4 5 6))))

(let ((xs (list 0)))
  (sequence-fill! xs '* 0 0)
  (assert (equal? xs '(0))))
(let ((xs (list 0 1 2 3)))
  (sequence-fill! xs '* 2)
  (equal? xs '(0 1 * *)))

(assert (equal? (sequence-append) (vector)))
(assert (string=? (sequence-append "" '#(#\a #\b) "c" '())
                  "abc"))

(assert (string=? (sequence-map char-upcase "abcdefg" 3)
                  "DEFG"))
(assert (equal? (sequence-map/maker make-vector
                                    - '(0 1 2 3 4 5) 2 4)
                '#(-2 -3)))
(assert (equal? (sequences-map - '#(3 2 1 0) '(2 1 0))
                '#(1 1 1)))

(assert (equal? (sequence-map-into! (vector 0 1 2 3 4 5 6) values
                                    "abcdefghijk")
                '#(#\a #\b #\c #\d #\e #\f #\g)))
(assert (equal? (sequence-map-into! (list 0 1 2 3 4 5 6) values
                                    "abcdefghijk")
                '(#\a #\b #\c #\d #\e #\f #\g)))
(assert (equal? (sequence-map-into! (list 0 1 2 3 4 5 6) values
                                    "abcdefghijk" 1 6 2)
                '(0 #\c #\d #\e #\f #\g 6)))
(let ((s (vector 0 1 2 3 4 5 6)))
  (assert (equal? (sequence-map-into! s values s 1 4 3)
                  '#(0 3 4 5 4 5 6))))

(assert (equal? (sequences-map-into! (list 'a 'b 'c 'd) +
                                     '#(0 1 2 3 4 5) '#(3 2 1 0))
                '(3 3 3 3)))
(assert (equal? (sequences-map-into! "" char-upcase "fno" "g")
                ""))

(assert (equal? (sequence-fold cons '() "abcde" 2 4)
                '(#\d #\c)))
(assert (equal? (sequence-fold-right cons '() "abcde" 2 4)
                '(#\c #\d)))
(assert (= (sequences-fold + 0 '(0 1 2 3) '#(1 2))
           4))
(assert (= (sequences-fold-right + 0 '(0 1 2 3) '#(1 2))
           4))

(assert (sequence= char=? "abcdefg" '#(#\b #\c #\d #\e) 1 5))
(assert (sequence= char=? "abcdefg" '#(#\b #\c #\d #\e f) 1 5 0 4))
(assert (sequence= = '() ""))

(assert (sequences= =))
(assert (sequences= = '(1)))
(assert (sequences= = '(1 2 3) '#(1 2 3) '(1 2 3)))
(deny (sequences= = '(1) '#(1 2)))

(assert (equal? (sequence-tabulate! (vector 1 2 3) 1 - 2)
                '#(1 0 -1)))
(assert (equal? (sequence-tabulate! (vector 1 2 3) 1 - 0)
                '#(1 2 3)))

(assert (sequence-null? ""))
(assert (sequence-null? '()))
(assert (sequence-null? '#()))
(deny (sequence-null? " "))
