(define-interface SPAN-interface
  (export
   make-SPAN-server
   make-SPAN-client))

(define-structure SPAN 
  schemedoc-interface
  (open scheme)
  (files SPAN-client SPAN-server SPAN))

