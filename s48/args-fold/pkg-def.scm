(define-package "srfi-37"
  (0 6)
  ()
  (install-file "load.scm" 'base)
  (install-file "README" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "args-fold.scm" 'scheme))
