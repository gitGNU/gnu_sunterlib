(define-structure overlapping-imports? (export)
  (open scheme
	optimizer
	signals 
	general-tables 
	packages-internal)
  (files overlapping-imports))