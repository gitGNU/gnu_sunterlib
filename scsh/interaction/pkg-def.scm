(define-package "interaction"
  (0 3)
  ((install-lib-version (1 3 0)))
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "packages.scm" 'scheme)
  (install-file "inspect-exception.scm" 'scheme)
  (install-file "repl.scm" 'scheme)
  (install-file "socket2stdport.scm" 'scheme))
