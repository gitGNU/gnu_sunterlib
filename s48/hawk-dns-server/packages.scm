(define-interface cavespider-interface
  (export
   make-server
   make-server-dns))

(define-structure cavespider 
  cavespider-interface
  (open scheme)
  (files load file-util hash-util html-util string-util util client))
