; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

(define-interface image-info-face
  (export image-dimension
          get-image-info

          image-info:format
          image-info:depth
          image-info:width/pixel
          image-info:height/pixel
          image-info:width/dpi
          image-info:height/dpi

          inport->byte-stream
          byte-vector->byte-stream
          segment-byte-stream
          ))
