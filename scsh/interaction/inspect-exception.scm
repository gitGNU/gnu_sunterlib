;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

;; From SUnet plus one more call/cc to capture the continuation of the error
(define (with-fatal-and-capturing-error-handler handler thunk)
  (call-with-current-continuation
   (lambda (accept)
     ((call-with-current-continuation
       (lambda (k)
	 (with-handler 
	  (lambda (condition more)
	    (primitive-cwcc
	     (lambda (condition-continuation)
	       (if (error? condition)
		   (call-with-current-continuation
		    (lambda (decline)
		      (k (lambda () 
			   (handler condition condition-continuation decline))))))
	       (more))))		; Keep looking for a handler.
	  (lambda () (call-with-values thunk accept)))))))))

(define (with-inspecting-handler port prepare thunk)
  (with-fatal-and-capturing-error-handler
   (lambda (condition condition-continuation more)
     (with-handler
      (lambda (c2 m2)
        (more))      
      (if (prepare condition)
          (let ((res
                 (remote-repl "Welcome to the command processor of the remote scsh"
                              condition-continuation
                              port)))
            ;; TODO: option to return to continuation of handler (by leaving out the with-continuation)
            (with-continuation condition-continuation (lambda () res)))
          (more))))
      thunk))

(define display-preview (eval 'display-preview 
                              (rt-structure->environment (reify-structure 'debugging))))

(define (display-continuation continuation . maybe-port)
  (let ((out (if (null? maybe-port)
                           (current-output-port)
                           (car maybe-port))))
    (if continuation
        (display-preview (continuation-preview continuation)
                         out)
        (display 'bottom-contination out))))
      