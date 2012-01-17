(define-interface SPAN-interface
  (export
   SPAN-question~))

(define-structure SPAN 
  schemedoc-interface
  (open scheme)
  (files SPAN-client SPAN-server SPAN load SPAN-util))

