(define-structure procedure-hash procedure-hash-interface
  (open scheme
        more-types
        loopholes
        closures
        disclosers)
  (files procedure-hash))

(define-structure procedure-tables procedure-tables-interface
  (open scheme
        tables
        procedure-hash)
  (files procedure-tables))
