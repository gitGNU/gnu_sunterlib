(define-interface scgame-interface
  (export
   make-scgame))

(define-structure scgame
  scgame-interface
  (open scheme)
  (files scgame))

