;;; blowfish.scm - blowfish encrypt and decrypt
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

(define (make-dictionary1)
  ;; methods are FIFO (first fixed first out)
  (let ((*dict '()))

    (define (get key) ;; get key
      (do ((l *dict (cdr l)))
	  ((eq? key (caar l))
	   (cadar l));;returns value
	))

    (define (ref-with-index i) ;; get key
      (do ((j 0 (+ j 1))
           (l *dict (cdr l)))
          ((= j i)
           (cadar l));;returns value
        ))

    (define (set-with-index i value) ;; set value
      (do ((j 0 (+ j 1))
           (l *dict (cdr l)))
          ((= j i)
           (set-car! (list-ref *dict j) value));;sets value FIXME
        ))

    (define (get-substring key) ;; get key
      (do ((l *dict (cdr l)))
	  ((string<=? (if (symbol? key)
                          (symbol->string key)
                          (if (string? key)
                              key
                              (display "dictionary-get-substring : unknown key type")))
                      (symbol->string (caar l)))
           (cadr l));;returns value
	))

    (define (add key value)
      (set! *dict (append *dict (list (list key value)))))

    (define (set key value) ;; get key
      (do ((l *dict (cdr l))
	   (res '() (append (list (car l) res))))
	  ((eq? key (caar l))
	   (set-car! (cdr res) value)
	   (set! *dict (append res (cdr l))))
	))


    (lambda (msg)
      (cond ((eq? msg 'get) get)
            ((eq? msg 'ref-with-index) ref-with-index)
            ((eq? msg 'set-with-index) set-with-index)
            ((eq? msg 'get-substring) get-substring)
	    ((eq? msg 'set) set)
	    ((eq? msg 'add) add)
	    (else (display "make-dictionary"))))
    ))

(define make-dictionary make-dictionary1)
(define (dictionary-ref dict key) ((dict 'get) key))
(define (dictionary-ref-with-index dict i) ((dict 'ref-with-index) i))
;; NOTE: dictionary-ref-substring:  match key part with keys in dict
(define (dictionary-ref-substring dict key) ((dict 'get-substring) key))
(define (dictionary-set! dict key value) ((dict 'set) key value))
(define (dictionary-add! dict key value) ((dict 'add) key value))
(define (dictionary-set-with-index! dict i value) ((dict 'set-with-index) i value))
