(define-interface base64-interface
  (export base64-encode-vector
          base64-encode-port
          base64-encode-string
          base64-decode-string
          base64-decode-port))

(define-structure base64 base64-interface
  (open scheme-with-scsh
        let-opt
        byte-vectors
        bytio)
  (files base64))
