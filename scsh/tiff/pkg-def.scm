(define-package "tiff"
  (1 0)
  ()
  (write-to-load-script
   `((config)
     (load ,(absolute-file-name "packages.scm"
                                (get-directory 'scheme #f)))))
  (install-file "README" 'doc)
  (install-file "NEWS" 'doc)
  (install-string (COPYING) "COPYING" 'doc)
  (install-file "aux.scm" 'scheme)
  (install-file "tiff-prober.scm" 'scheme)
  (install-file "vtiff.scm" 'scheme)
  (install-file "endian.scm" 'scheme)
  (install-file "tiff.scm" 'scheme)
  (install-file "gnu-head-sm.tif" 'misc-shared)
  (install-file "bsp.tiff" 'misc-shared))