(define-interface scratch-interface
  (export
   run-daemon-child-http))

(define-structure thttpd
  scratch-interface
  (open scheme)
  (files scratch))
