(define-package "banana"
  (0 1)
  ()
  (install-file "load.scm" 'base)
  (install-file "README" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "banana.scm" 'scheme))
