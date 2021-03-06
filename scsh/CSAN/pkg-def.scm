(define-package "CSAN"
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
  (install-file "CSAN-client.scm" 'scheme)
  (install-file "CSAN-server.scm" 'scheme)
  (install-file "CSAN-server-daemon.scm" 'scheme)
  (install-file "CSAN-server-daemon-record.scm" 'scheme)
  (install-file "CSAN-util.scm" 'scheme)
  (install-file "load.scm" 'scheme)
  (install-file "CSAN.scm" 'scheme))
