(define-interface CSAN-interface
  (export
   CSAN-question~
   CSAN-shell-spawn
   CSAN-dir))

(define-structure CSAN
  CSAN-interface
  (open scheme)
  (files CSAN-client CSAN-server CSAN load CSAN-util))

