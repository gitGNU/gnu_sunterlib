(define tifflet-face
  (export read-tiff-file
          print-tiff-directory
          tiff-directory-get
          tiff-directory-get-as-symbol
   ))

(define tifftag-face
  (export tagdict-get-by-name
          tagdict-get-by-num
          tagdict-tagval-get-by-name
          tagdict-tagval-get-by-num
          make-tagdict
          tagdict?
          tagdict-add-all
          tiff-standard-tagdict
          ))

(define tiffdir-face
  (export tiff-directory?
          tiff-directory-size
          tiff-directory-empty?
          tiff-directory-get
          tiff-directory-get-as-symbol
          read-tiff-file
          print-tiff-directory
          tiff-directory-fold-left
          tiff-dir-entry?
          tiff-dir-entry-tag
          tiff-dir-entry-type
          tiff-dir-entry-count
          tiff-dir-entry-val-offset
          tiff-dir-entry-value
          print-tiff-dir-entry
          ))

(define tiff-prober-face
  (export tiff-prober))

(define-interface endian-face
  (export make-endian-port
          close-endian-port
          endian-port-set-bigendian!
          endian-port-set-littlendian!
          endian-port-read-int1
          endian-port-read-int2
          endian-port-read-int4
          endian-port-setpos))

;;;

(define-interface ersatz-srfi-4-face
  (export u8vector?
          u8vector  make-u8vector
          u8vector-length
          u8vector-ref u8vector-set!
          u8vector->list list->u8vector

          u16vector?
          u16vector  make-u16vector
          u16vector-length
          u16vector-ref u16vector-set!
          u16vector->list list->u16vector

          u32vector?
          u32vector  make-u32vector
          u32vector-length
          u32vector-ref u32vector-set!
          u32vector->list list->u32vector
          ))


(define-interface tiff-helpers-face
  (export (define-structure :syntax)
          (++ :syntax)
          cerr cout nl))


