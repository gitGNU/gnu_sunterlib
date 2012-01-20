(define-interface encryption-interface
  (export
   blowfish-encrypt
   blowfish-encrypt
   blowfish-encrypt-block
   blowfish-decrypt-block))

(define-structure schemedoc 
  schemedoc-interface
  (open scheme)
  (files blowfish dictionary))

