;;; SPAN-util.scm - Scheme Perl Archive Network utilities
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

(define (url-bite-off url)
  (let ((s "")
        (do ((i 0 (+ i 1)))
            ((>= i (string-length url))
             ;;(if (= (string-ref url i) #\/)
             ;;    (set!
             s)
          (set! s (string-append s (string (string-ref url i)))))
        (if (or (eq? s "http://")(eq? s "ftp://"))
            (set! s "")))))

(define (SPAN-shell-spawn mirror)
  (do ((s (read)(read)))
      ((null? s)0)
    (cond ((symbol? s)
           (cond ((string<=? (symbol->string s)(string #\return))
                  0)
                 ((string=? "h" (symbol->string s))
                  (display "Commands : get ")
                  0)
                 ((string<=? "get" (symbol->string s))
                  (display "enter package to fetch : ")
                  (SPAN-ask-server (string-append "get " (symbol->string (read)))
                                   (url-bite-off mirror) 6969))
                 ))
          ))
  (display "Signing off."))