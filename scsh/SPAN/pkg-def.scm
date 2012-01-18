(define-package "SPAN"
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
  (install-file "SPAN-client.scm" 'scheme)
  (install-file "SPAN-server.scm" 'scheme)
  (install-file "SPAN-server-daemon.scm" 'scheme)
  (install-file "SPAN-server-daemon-record.scm" 'scheme)
  (install-file "SPAN-util.scm" 'scheme)
  (install-file "load.scm" 'scheme)
  (install-file "SPAN.scm" 'scheme))
