(define-structure mini-profiler mini-profiler-interface
  (open scheme
	table
	formats
	extended-ports
	time)
  (files profile))

(define-structure no-mini-profiler mini-profiler-interface
  (open scheme)
  (files no-profile))
