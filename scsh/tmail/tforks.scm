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

(load "util.scm")
(load "trecords.scm")
(load "tmailbox-load.scm")

(define (run-daemon-child-mail rec)
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

    (for-each display '("Opening listening socket on host : "
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
           (do ((i 0 (+ i 1))) ;; iterate
               ((> i 9)(display *bye)(newline))
             (call-with-values
                 (lambda ()
                   '((read in)(read in)(read in)))
               (lambda (answer)
                 (let ((answer2 (car answer))
                       (answer3 (if (not (null? (cdr answer)))
                                    (cadr answer)
                                    'foo)))
                   (for-each display '((servermsg) (symbol->string answer2) " "  answer3 (eoln)))
                   (if (symbol? answer2)
                       (cond ((eq? 'APOP answer2)
                              (write mailbox-contents out)
                              )
                             ((eq? 'STAT answer2)
                              #t)
                             ((eq? 'LIST answer2)
                              #t)
                             ((eq? 'RETR answer2)
                              (let ((idx (string->number (symbol->string answer3))))
                                (if idx
                                    (write (tmail-get-mail-with-index (getenv "USER") idx) out))))
                             ((eq? 'USER answer2)
                              (let ((username (if (number? answer3)
                                                  (number->string answer3)
                                                  (symbol->string answer3))))
                                (setenv "USER" username)
                                (write "+OK user accepted - not implemented" out)))
                             ((eq? 'PASS answer2)
                              (let ((pass (if (number? answer3)
                                              (number->string answer3)
                                              (symbol->string answer3))))
                                (write "+OK password accepted - not implemented" out)))
                             ((eq? 'QUIT answer2)
                              (write *bye out)
                              (close-input-port in)
                              (close-socket *socket)
                              (close-output-port out)
                              (exit))
                             (else
                              (write (errormsg) out))
                             ))
                   ))))
           (write *bye out)
           (close-input-port in)
           (close-socket *socket)
           (close-output-port out)
           (exit)
           ))))))