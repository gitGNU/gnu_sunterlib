(define-package "exceptions"
  (0 1)
  ()
  (install-file "load.scm" 'base)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "restart.scm" 'scheme)
  (install-file "srfi-34.scm" 'scheme)
  (install-file "srfi-35.scm" 'scheme))
