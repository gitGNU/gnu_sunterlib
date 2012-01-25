(define-interface CSAN-interface
  (export
   CSAN-question~))

(define-structure CSAN
  schemedoc-interface
  (open scheme)
  (files CSAN-client CSAN-server CSAN load CSAN-util))

