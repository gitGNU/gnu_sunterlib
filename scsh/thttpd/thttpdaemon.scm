;;; thttpdaemon.scm - a scheme web daemon
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

;;,open ascii records sockets continuations

(define eoln (string #\newline))
(define cr (string (ascii->char 13)))
(define servermsg "::thttpd-msg::")
(define errormsg "::thttpd-error::")
(define aspect-content (string-append "Content-Type: text/plain;charset=utf-8" cr eoln))
(define :thttpd-daemon-record
  (make-record-type 'thttpd-daemon-record
		    '(hostname port sock)))
(define make-thttpd-daemon-record
  (record-constructor :thttpd-daemon-record
		      '(hostname port sock)))

(define thttpd-hostname (record-accessor :thttpd-daemon-record 'hostname))
(define thttpd-port (record-accessor :thttpd-daemon-record 'port))
(define thttpd-sock (record-accessor :thttpd-daemon-record 'sock))

(define (get-response-f lst)
  (define (get return)
    (for-each
     (lambda (element)
       (set! return (call-with-current-continutation
                     (lambda (r)
                       (set! get r)
                       (return element)))))
     lst)
    (return 'end-generate))

  (define (gen)
    (call-with-current-continuation get))
  gen)

(define (get-response l)
  (get-response-f l))

(define (run-daemon-child-http rec)
  (let ((*hostname (thttpd-hostname rec))
        (*port (thttpd-port rec))
        (*socket (thttpd-sock rec))
        )

    (set! *socket (open-socket *port))

    (for-each display '("Opening listening socket on host : "
                        *hostname
                        " port : "
                        *port
                        eoln))
    ((lambda ()
       (call-with-values
           (lambda ()
             (socket-accept *socket))
         (lambda (in out)
           (let* ((a (read in));;race cond. client requests
                  (b (read in))
                  (c (read in))
                  (abcl '(a b c)))
             ;;(let ((in (make-string-input-port in)))
             (for-each display '(servermsg (symbol->string a)))
             (if (symbol? a)
                 (let ((response-word (get-response abcl)))
                   (cond ((eq? a 'GET)
                          ;; fall through with continuations
                          (let ((response-word-2nd (get-response abcl)))
                            (if (symbol? response-word-2nd)
                                (begin
                                  ;;(display "200 OK" out)
                                  (display aspect-content out)
                                  ;; FIXME #\return (make-char X)
                                  (display (string-append cr eoln) out) ;; CRLF
                                  (display "\"Hello World\"" out)
                                  ))))
                                  ;;(close-input-port in)
                                  ;;(close-socket *socket)
                                  ;;(close-output-port out)
                         (else ;; + keep-alive
                          (write errormsg out))
                         ))))))))))