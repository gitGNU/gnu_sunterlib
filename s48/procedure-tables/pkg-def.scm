(define-package "procedure-tables"
  (0 0)
  ()
  (install-file "load.scm" 'base)
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "procedure-tables.scm" 'scheme)
  (install-file "procedure-hash.scm" 'scheme))
