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

(load "schemedoc.scm")

;;
;; main program
;;

(define ask (getenv "SCHEMEDOCDIR"))
(case ask
  ((#f) (begin (for-each
                display
                '("set your SCHEMEDOCDIR env var to the paths where pods and sods reside." (eoln) "exiting" (eoln)))
               (exit) ;; NOTE exit
               )))


;;(for-each schemedoc-parser-grep SCHEMEDOCDIR)
(define directoriesl (schemedoc-get-env-list SCHEMEDOCDIR))
(for-each display directoriesl)
(display directoriesl)
(do ((l directoriesl (cdr l)))
    ((null? l)
     0)
  (display (directory-files (car l)) )
  (do ((l2 (directory-files (car l)) (cdr l2)))
      ((null? l2)0)
    (schemedoc-parser-grep (car l2)))
  )



;;(for-each directory-files directoriesl)
