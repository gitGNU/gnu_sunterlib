(define-package "procedure-tables"
  (0 0)
  ()
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "procedure-tables.scm" 'scheme)
  (install-file "procedure-hash.scm" 'scheme))