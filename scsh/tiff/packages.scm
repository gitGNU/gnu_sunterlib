(define-structures
  ((tifflet tifflet-face)
   (tiff (compound-interface tifftag-face tiffdir-face)))
  (open tiff-helpers endian
        krims                           ; assert
        (modify sequence-lib (rename (sequence-any any?)))  ; any?
        ascii                           ; ascii->char
        srfi-11                         ; let*-values
        srfi-23                         ; error
        ersatz-srfi-4
        scheme)
  (files tiff))

(define-structure endian endian-face
  (open tiff-helpers                    ; define-structure
        ascii                           ; char->ascii
        srfi-23                         ; error
        (modify scheme-with-scsh (rename (seek OS:fseek-abs))))
                                        ; seek, bit-ops, buffer policy
  (begin (define char->integer char->ascii))
  (files endian))

(define-structure tiff-helpers tiff-helpers-face
  (open srfi-9
        srfi-23                         ; error
        scheme-with-scsh                ; error-output-port
        )
  (for-syntax (open scheme))
  (files aux))

(define-structure ersatz-srfi-4 ersatz-srfi-4-face
  (open (modify scheme (alias (vector? u8vector?)
                              (vector u8vector)
                              (make-vector make-u8vector)
                              (vector-length u8vector-length)
                              (vector-ref u8vector-ref)
                              (vector-set! u8vector-set!)
                              (vector->list u8vector->list)
                              (list->vector list->u8vector)

                              (vector? u16vector?)
                              (vector u16vector)
                              (make-vector make-u16vector)
                              (vector-length u16vector-length)
                              (vector-ref u16vector-ref)
                              (vector-set! u16vector-set!)
                              (vector->list u16vector->list)
                              (list->vector list->u16vector)

                              (vector? u32vector?)
                              (vector u32vector)
                              (make-vector make-u32vector)
                              (vector-length u32vector-length)
                              (vector-ref u32vector-ref)
                              (vector-set! u32vector-set!)
                              (vector->list u32vector->list)
                              (list->vector list->u32vector)
                              ))))

(define-structure tiff-prober tiff-prober-face
  (open tifflet
        endian                          ; make-endian-port
        tiff-helpers                    ; cout cerr nl
        scheme-with-scsh                ; scsh for open-input-file
        )
  (begin (define (exit) #f))            ; good enough
  (files tiff-prober))

(define-structure tiff-testbed (export )
  (open tiff
        endian                          ; endian ports
        tiff-helpers                    ; cerr nl ++
        krims                           ; assert
        ersatz-srfi-4                   ; fake uniform vectors
        srfi-11                         ; let-values*
        scheme-with-scsh
        )
  (begin ))

