(define-interface twentyseventree-interface
  (export
   make-twentyseventree))

(define-structure twentyseventree
  twentyseventree-interface
  (open scheme)
  (files twentyseventree))

