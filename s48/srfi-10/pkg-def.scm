(define-package "srfi-10"
  (0 0)
  ()
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "srfi-10.scm" 'scheme)
  (install-file "test/math.scm" 'scheme)
  (install-file "test/pi.scm" 'scheme))
