(define-package "scsh-packages"
  (0 0)
  ()
  ; (install-file "load.scm" 'base)
  ; (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "scheme/install-lib/configure.scm" 'scheme)
  (install-file "scheme/install-lib/install-lib-module.scm"
                'scheme)
  (install-file "scheme/install-lib/install-lib.scm" 'scheme)
  (install-file "scheme/install-lib/install-pkg" 'scheme)
  (install-file "doc/latex/proposal.tex" 'misc-shared))
