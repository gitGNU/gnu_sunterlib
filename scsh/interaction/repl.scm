;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define (bind-listen-accept protocol-family proc arg)
      (let* ((sock (create-socket protocol-family socket-type/stream))
	     (addr (cond ((= protocol-family
			     protocol-family/internet)
			  (let ((port (cond ((integer? arg) arg)
					    ((string? arg)
					     (service-info:port 
					      (service-info arg "tcp")))
					    (else
					     (error "socket-connect: bad arg ~s"
						    arg)))))
			    (internet-address->socket-address internet-address/any
							      arg)))
			 ((= protocol-family
			     protocol-family/unix)
			  (unix-address->socket-address arg))
			 (else 
			  (error "bind-listen-accept: unsupported protocol-family ~s"
				 protocol-family)))))
	(set-socket-option sock level/socket socket/reuse-address #t)
	(bind-socket sock addr)
	(listen-socket sock 5)
	(with-handler
	 (lambda (condition more)
	   (with-handler
	    (lambda (condition ignore) (more))
	    (lambda () (close-socket sock)))
	   (more))
	 (lambda ()
	   (with-errno-handler
	    ;; ECONNABORTED we just ignore
	    ((errno packet)
	     ((errno/connaborted) 'fick-dich-ins-knie))
	    (call-with-values 
	     (lambda () (accept-connection sock))
	     proc))))))

(define (remote-repl greeting focus-value port)
  (let ((old-output (command-output))
	(old-input (command-input))
	(old-err (command-error-output)))
    (bind-listen-accept
     protocol-family/internet
     (lambda (socket address)
       (set-port-buffering (socket:outport socket) bufpol/none)
       (set-port-buffering (socket:inport socket) bufpol/none)
       (set-socket-option socket level/socket socket/oob-inline #t)
       (set-socket-option socket level/socket socket/reuse-address #t)
       (let ((res (script-repl greeting
			       focus-value
			       (socket:inport socket)
			       (socket:outport socket)
			       (socket:outport socket))))
	 (with-handler
	  (lambda (condition ignore) 'fick-dich-ins-knie)
	  (lambda () (close-socket socket)))
	 res))
     port)))

(define (script-repl greeting focus-value iport oport eport)
  (with-interaction-environment
   (environment-for-commands)
   (lambda ()
     (let* ((saved-user-context (user-context))
	    (res
	     (restart-command-processor
	      'ignored-focus-value
	      (user-context)
	      (lambda ()
		;; we need to redirect the ports of the command processor
		(start-new-session saved-user-context
				   iport
				   oport
				   eport
				   focus-value
				   #f)	;batch?
		(display greeting (command-output))
		(newline (command-output))
		(display (focus-object) (command-output))
		(newline (command-output))))))
       (format (command-output) "Terminating command processor with value ~A ~%" res)
       res))))
