(define-package "profile"
  (0 0)
  ()
  (install-file "load.scm" 'base)
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "no-profile.scm" 'scheme)
  (install-file "profile.scm" 'scheme))
