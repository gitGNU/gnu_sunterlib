;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define (socket<->stdports host port)
  (let ((s (socket-connect protocol-family/internet socket-type/stream host port)))
    (set-port-buffering (socket:outport s) bufpol/none)
    (set-port-buffering (socket:inport s) bufpol/none)
    (spawn (lambda () (dynamic-wind
		       (lambda () #f)
		       (lambda ()
			 (dup-port (socket:inport s) (current-output-port)))
		       (lambda ()
			 (close (socket:inport s))))))

    (dynamic-wind
     (lambda () #f)
     (lambda ()
       (dup-port (current-input-port) (socket:outport s)))
     (lambda ()
       (close-socket s)))))


(define (dup-port from to)
  (let ((c (read-char from)))
    (if (not (eof-object? c))
	(begin 
	  (display c to)
	  (dup-port from to)))))


