(define-structure base64 base64-interface
  (open scheme-with-scsh
        let-opt
        byte-vectors
        bytio)
  (files base64))
