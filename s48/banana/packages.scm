(define-structures ((banana banana-interface)
                    (banana-extras banana-extras-interface))
  (open scheme
        define-record-types
        conditions
        exceptions
        signals
        thread-fluids
        bitwise
        tables
        handle
        byte-vectors
        ascii
        extended-ports
        srfi-1
        srfi-2
        srfi-16)
  (files banana))
