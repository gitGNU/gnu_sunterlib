(define-package "cml"
  (0 0)
  ()
  (install-file "load.scm" 'base)
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "async-channels.scm" 'scheme)
  (install-file "placeholder.scm" 'scheme)
  (install-file "trans-id.scm" 'scheme)
  (install-file "channel.scm" 'scheme)
  (install-file "jar.scm" 'scheme)
  (install-file "rendezvous.scm" 'scheme))








