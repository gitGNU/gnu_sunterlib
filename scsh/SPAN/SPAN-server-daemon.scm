;;; tforks.scm - a scheme daemon child process
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

;; NOTE : files stored on this server are retrieved from its runtime directory

(load "SPAN-server-daemon-record.scm")

(define (errormsg) (display " message not understood. "))
(define (eoln) (string #\newline))
(define (servermsg) (display "::message::"))

(define (get-package package-name)
  (let* ((in (open-input-file (if (string<=? ".tar.gz" package-name)
                                  package-name
                                  (string-append package-name ".tar.gz"))))
         (contents ""))
    (do ((c (read-char in) (read-char in)))
        ((eof-object? c)contents)
      (set! contents (string-append contents (string c))))))

(define (run-daemon-child-SPAN rec)
  (let ((*hostname (hostname rec))
        (*port (port rec))
        (*socket (socket rec))
        (*motd (motd rec))
        (*ack (ack rec))
        (*apop (apop rec))
        (*stat (stat rec))
        (*list (list rec))
        (*bye (bye rec)))

    (set! *socket (open-socket *port))

    (for-each display '("Opening SPAN server side : listening on host : "
                        *hostname
                        " port : "
                        *port
                        (eoln)))

    ((lambda ()
       (call-with-values
           (lambda ()
             (socket-accept *socket))
         (lambda (in out)
           (write *motd out)
           (let ((answer (read (make-string-input-port in))))
             (for-each display '((servermsg) (symbol->string answer)))
             (if (symbol? answer)
                 (cond ((eq? 'get answer)
                        (let ((answer2 (read (make-string-input-port in))))
                          (write (get-package answer2) out)))
                       ((or (eq? 'QUIT answer)(eq? 'quit answer))
                        (write *bye out)
                        (close-input-port in)
                        (close-socket *socket)
                        (close-output-port out)
                        (exit))
                       (else
                        (write (errormsg) out))
                       ))
             ))))
     (let ((answer2 (read (make-string-input-port in))))
       (for-each display '((servermsg) (symbol->string answer2)))
       (write *bye out)
       (close-input-port in)
       (close-socket *socket)
       (close-output-port out)
       (exit)))))


(define rc (make-daemon-record
	    "localhost" ;; virtual host
	    6969
            #f
	    "SPAN server side ready."
	    "Scheming..."
            "SPAN server signing off."
            ))

(run-daemon-child-SPAN rc)
