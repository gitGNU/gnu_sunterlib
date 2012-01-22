(define-interface thttpd-interface
  (export
   run-daemon-child-http))

(define-structure thttpd
  tmail-interface
  (open scheme)
  (files thttpdaemon load))
