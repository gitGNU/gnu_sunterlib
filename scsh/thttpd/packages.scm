(define-interface thttpd-interface
  (export
   run-daemon-child-http))

(define-structure thttpd
  thttpd-interface
  (open scheme)
  (files thttpdaemon load))
