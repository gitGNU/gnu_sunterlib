(define-interface repls-interface
  (export script-repl
	  remote-repl))

(define-interface inspect-exception-interface
  (export with-inspecting-handler))

(define-interface socket2stdports-interface
  (export socket<->std-ports))
