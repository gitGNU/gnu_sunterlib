(define-package "concurrency"
  (0 0)
  ()
  (install-file "load.scm" 'base)
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "semaphore.scm" 'scheme)
  (install-file "with-lock.scm" 'scheme))
