(define-structures ((banana banana-interface)
                    (banana-extras banana-extras-interface))
  (open scheme
        define-record-types
        conditions
        exceptions
        signals
        handle
        bitwise
        byte-vectors
        ascii
        srfi-1
        srfi-6)
  (files banana))
