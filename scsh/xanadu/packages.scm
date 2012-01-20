(define-interface scgame-interface
  (export
   make-scganadu))

(define-structure scgame
  scgame-interface
  (open scheme)
  (files load scganadu scganaduutil))

