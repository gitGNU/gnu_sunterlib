(define-package "sequences"
  (1 0)
  ()
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "baseqs.scm" 'scheme)
  (install-file "genseqs.scm" 'scheme)
  (install-file "uniseqs.scm" 'scheme)
  (install-file "composeqs.scm" 'scheme)
  (install-file "specseqs.scm" 'scheme)
  (install-file "vecnames.scm" 'scheme))
