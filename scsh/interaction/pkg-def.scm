(define-package "interaction"
  (0 0)
  ()
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "inspect-exception.scm" 'scheme)
  (install-file "repl.scm" 'scheme)
  (install-file "socket2stdport.scm" 'scheme))
