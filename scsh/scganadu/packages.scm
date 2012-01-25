(define-interface scganadu-interface
  (export
   make-scganadu))

(define-structure scganadu
  scgame-interface
  (open scheme)
  (files load scganadu scganaduutil))

