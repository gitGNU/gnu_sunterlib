;;; scganaduutil.scm - a scheme Xanadu utility
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

(load "scgameutil.scm")

(define (make-copy-of-document)
  (let ((*XMLOPENDATA (list "<scganadu>" "<audio>" "<image>"))
	(*XMLCLOSEDATA (list "</scganadu>" "</audio>" "</image>"))
	(*scganadutag! car)
	(*audiotag! cadr)
	(*imagetag! caadr)
	)

    (define (get-open-xml tag)
      (tag *XMLOPENDATA))
    (define (get-close-xml tag)
      (tag *XMLCLOSEDATA))

  (define (get-copyright)
    "Copyright (C) unknown by SCGanadu."))

  (define (get-post hypertext)
    (get-post-html hypertext))
  (define (get-post-html hypertext)
    (string-append (get-open-xml scganadutag!) hypertext (get-open-xml scganadutag!))
  (define (get-post-sound hypertext2)
    (get-post-html (string (get-open-xml audiotag!) hypertext2 (get-close-xml audiotag!))))
  (define (get-post-image hypertext3)
    (get-post-html (string (get-open-xml imagetag!) hypertext3 (get-close-xml imagetag!))))

  (lambda (msg)
    (cond ((eq? msg 'get-copyright)
	   get-copyright)
	  ((eq? msg 'get-post-sound)
	   get-post-sound)
	  ((eq? msg 'get-post-image)
	   get-post-image)
	  ((eq? msg 'get-post-scganadu)
	   get-post-scganadu)
	  ((eq? msg 'get-post)
	   get-post)
	  (else (aspecterror) (display "make-copy-of-document")))))
