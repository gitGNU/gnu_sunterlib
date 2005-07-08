(define-package "dir-streams"
  (1 0)
  ((install-lib-version (1 2 0)))
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "packages.scm" 'scheme)
  (install-file "dir-stream.scm" 'scheme)
  (install-file "dir-stream-predicates.scm" 'scheme)
  (install-file "stream.scm" 'scheme))
