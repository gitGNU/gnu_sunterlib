(define-interface snow-interface
  (export
   parse-for snow-repository))

(define-structure snow 
  snow-interface
  (open scheme)
  (files snow))

