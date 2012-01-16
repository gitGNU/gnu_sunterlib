(define-package "scgame"
  (0 1)
  ((install-lib-version (1 3 0)))
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "packages.scm" 'scheme)
  (install-file "config.scm" 'scheme)
  (install-file "scgamedictionaries.scm" 'scheme)
  (install-file "scgameutil.scm" 'scheme)
  (install-file "scgamewidgets.scm" 'scheme)
  (install-file "scganadu.scm" 'scheme)
  (install-file "scganaduutil.scm" 'scheme)
  (install-file "scgame.scm" 'scheme))
