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
