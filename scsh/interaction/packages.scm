(define-interface repls-interface
  (export script-repl
	  remote-repl))

(define-interface inspect-exception-interface
  (export with-inspecting-handler
          with-fatal-and-capturing-error-handler
          display-continuation))

(define-interface socket2stdports-interface
  (export socket<->stdports))

(define-interface gc-interface
  (export collect
          gc-count))

(define-structure repls repls-interface
  (open scheme-with-scsh
	command-levels
	command-processor
	environments
	formats
	handle)
  (files repl))

(define-structure inspect-exception inspect-exception-interface

  (open scheme-with-scsh
        rt-modules
        exceptions
	conditions
	escapes
	handle
	repls)
  (files inspect-exception))

(define-structure socket2stdports socket2stdports-interface
  (open scheme-with-scsh
	handle
	threads)
  (files socket2stdport))

(define-structure gc gc-interface
  (open scheme
        enumerated
        (subset architecture (memory-status-option))
        (modify primitives (rename (collect primitive-collect))
                           (expose collect memory-status))
        threads)
  (files gc))
