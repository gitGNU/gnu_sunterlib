(define-package "module-system"
  (1 0)
  ()
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "packages.scm" 'scheme)
  (install-file "overlapping-imports.scm" 'scheme)
  (install-file "rt-module.scm" 'scheme))
