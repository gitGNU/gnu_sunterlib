(define-interface mini-profiler-interface
  (export
   profile-init!
   display-profile
   (define-prof :syntax)
   (account-for :syntax)))