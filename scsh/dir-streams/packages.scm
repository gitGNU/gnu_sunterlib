(define-structure dir-streams dir-streams-interface
  (open scheme-with-scsh
	handle
	conditions
	define-record-types
	let-opt
	records
	streams)
  (files dir-stream))

(define-structure dir-stream-predicates dir-stream-predicates-interfaces
  (open 
   scheme-with-scsh
   dir-streams)
  (files dir-stream-predicates))

(define-structure streams
  streams-interface
  (open scheme
	signals)
  (files stream))
