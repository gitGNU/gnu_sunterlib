(define-package "scsh-packages"
  (0 0)
  ()
  ;(write-to-load-script
  ; `((config)
  ;   (load ,(absolute-file-name "packages.scm"
  ;                              (get-directory 'scheme #f)))))
  ; (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "scheme/install-lib/configure.scm" 'scheme)
  (install-file "scheme/install-lib/install-lib-module.scm"
                'scheme)
  (install-file "scheme/install-lib/install-lib.scm" 'scheme)
  (install-file "scheme/install-lib/install-pkg" 'scheme)
  (install-file "doc/latex/proposal.tex" 'misc-shared))
