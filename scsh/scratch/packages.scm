(define-interface scratch-interface
  (export
   char-continue))

(define-structure thttpd
  scratch-interface
  (open scheme)
  (files load scratch))
