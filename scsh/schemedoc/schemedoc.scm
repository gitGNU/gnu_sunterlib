;;; schemedoc.scm - a scheme perldoc utility
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

;;FIXME grepper for comments
;;FIXME use make-regexp in a record
(define sod-regexp1 (rx (| "=item")))

(define (sod regexp filename)
  (let ((in (open-input-file filename)))
    (let ((contents ""))
      (do ((s (read-char in)(read-char in)))
          ((eof-object? s) contents))
      (string-match regexp contents))))


(define (schemedoc-print-doc filename)
  (let ((l (list (sod (if (regexp? sod-regexp1)
                          sod-regexp1
                          (rx ("")))
                      filename))))
    (for-each display l)))

(define (schemedoc-print-doc-to-file filename outfilename)
  (let ((out (open-output-file outfilename)))
    (let ((l (list (sod (if (regexp? sod-regexp1)
                            sod-regexp1
                            (rx ("")))
                        filename))))
      (define (display-rec ll)
        (do ((e ll (cdr e)))
            ((null? e)0)
          (display (car e) out)))
      (display-rec l))))

;;
;; make a list of chars from filename contents
;;
(define (schemedoc-explode-doc filename)
  (define (explode in)
    (let ((c (read-char in)))
      (if (eof-object? c)
          c
          (append (list c) (explode in)))))

  (define (read-rec in)
    (call-with-values
        (lambda ()
          (explode in)
          )
      (lambda (l)
        (display l)
        l)))

  (let ((in (open-input-file filename)))
    (read-rec in)
    ))
