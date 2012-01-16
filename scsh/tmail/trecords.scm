;;; trecords.scm - records for tmail 
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

(define :daemon-record
  (make-record-type 'daemon-record 
		    '(hostname port sock MOTD ACK APOP STAT LIST BYE SPAWNEND ERROR500 HELO EHLO)))
(define make-daemon-record 
  (record-constructor :daemon-record 
		      '(hostname port sock MOTD ACK APOP STAT LIST BYE SPAWNEND ERROR500 HELO EHLO)))
;; (define make-daemon-record-default 
;;   (record-constructor :daemon-record 
;; 		      '(hostname 
;; 		 	1025 #f
;; 			"Hello."
;; 			"OK." 
;; 			"Bye." 
;; 			"500 Command not understood." 
;; 			"EHLO Server ready.")))
(define hostname (record-accessor :daemon-record 'hostname))
(define port (record-accessor :daemon-record 'port))
(define sock (record-accessor :daemon-record 'sock))
(define MOTD (record-accessor :daemon-record 'MOTD))
(define ACK (record-accessor :daemon-record 'ACK))
(define APOP (record-accessor :daemon-record 'APOP))
(define STAT (record-accessor :daemon-record 'STAT))
(define LIST (record-accessor :daemon-record 'LIST))
(define BYE (record-accessor :daemon-record 'BYE))
(define SPAWNEND (record-accessor :daemon-record 'SPAWNEND))
(define ERROR500 (record-accessor :daemon-record 'ERROR500))
(define HELO (record-accessor :daemon-record 'HELO))
(define EHLO (record-accessor :daemon-record 'EHLO))
					    
