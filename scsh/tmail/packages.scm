(define-interface tmail-interface
  (export
   run-daemon-child))

(define-structure tmail
  tmail-interface
  (open scheme)
  (files tdaemon trecords tclient tserver tforks tmailbox))