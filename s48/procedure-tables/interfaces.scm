(define-interface procedure-hash-interface
  (export (procedure-hash (proc (:procedure) :exact-integer))))

(define-interface procedure-tables-interface
  (export (make-procedure-table (proc () :value))))
