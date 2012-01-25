;;; scganadu.scm - a Xanadu file system (until desktop publishing)
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

;; This code fabricates xanadu hypertext files to attach
;; to xanandu objects or use as metafiles

(load "scganaduutil.scm")

(define (make-scganadu)
  (let ((record (delay #f)))

    (define (add-file filename)
      (let ((displayproc (write (((FILE-MAKER-unit (force record)) 'get-post-html)
				 (string-append "<filename>"
						filename
						"</filename>"))))

	    (with-output-to-file (string-append "." filename ".scganadu") displayproc)
	    )))

    (define (attach-to-file! filename)
      (let ((displayproc (write (((FILE-MAKER-unit (force record)) 'get-post-html)
				 (string-append "<filename>"
						filename
						"</filename>")))))

	(with-output-to-file filename displayproc)
	))

    (define (dispatch msg)
      (lambda (msg)
	(cond ((eq? msg 'add-file)add-file)
	      ((eq? msg 'attach-to-file!)attach-to-file!)
	      (else (aspecterror) (display "make-scganadu")))))


  (define :scganadu-record
    (make-record-type 'scganadu-record
		      '(FILE-MAKER make-scganadu)))
  (define make-scganadu-record
    (record-constructor :scganadu-record
			'(FILE-MAKER make-scganadu)))
  (define FILE-MAKER-unit (record-accessor :scganadu-record 'FILE-MAKER))
  (define make-scganadu-unit (record-accessor :scganadu-record 'make-scganadu))
  (define make-scganadu-record
    (delay (make-copy-of-document))
    (delay (make-cell dispatch)))
  (set! record make-scganadu-record)
  dispatch))


