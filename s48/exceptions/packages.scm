(define-structure srfi-34 srfi-34-interface
  (open scheme
	signals)
  (files srfi-34))

(define-structure srfi-35 srfi-35-interface
  (open scheme
	signals
	srfi-1
	srfi-9)
  (files srfi-35))

(define-structure srfi-34-restart srfi-34-restart-interface
  (open scheme
	srfi-34 srfi-35)
  (files restart))