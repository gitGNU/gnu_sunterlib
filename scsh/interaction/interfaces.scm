(define-interface repls-interface
  (export script-repl
	  remote-repl))

(define-interface inspect-exception-interface
  (export with-inspecting-handler
          with-fatal-and-capturing-error-handler
          display-continuation))

(define-interface socket2stdports-interface
  (export socket<->stdports))
