(define-package "pps"
  (0 0)
  ()
  (install-file "load.scm" 'base)
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "pps.scm" 'scheme))
