(define-structure pps pps-interface
  (open scheme-with-scsh
	(subset srfi-1 (drop take))
	define-record-types)
  (files pps))