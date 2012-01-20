(define-interface encryption-interface
  (export
   blowfish-encrypt
	blowfish-decrypt))

(define-structure schemedoc 
  schemedoc-interface
  (open scheme)
  (files blowfish dictionary))

