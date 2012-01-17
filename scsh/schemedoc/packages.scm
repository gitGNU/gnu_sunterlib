(define-interface schemedoc-interface
  (export
   schemedoc-print-doc))

(define-structure schemedoc 
  schemedoc-interface
  (open scheme)
  (files schemedoc))

