; Copyright (c) 2003 RT Happe <rthappe at web de>
; See the file COPYING distributed with the Scheme Untergrund Library

#!
Loose port of small parts from Marco Schmidt's public domain ImageInfo 1.3
Java program.  The original code is at
http://www.geocities.com/marcoschmidt.geo/image-info.html

Synopses

Extracting the image dimension etc.

  (image-dimension img) ==> [w h]
  extract the width W and height H (in pixels) from the byte source
  (input port, byte-vector, b.s.(*)) repesenting an image -- or signal 
  an error.  Supported formats: gif, jpeg, png.  

  (get-image-info bs) ==> info | #f
  extract information on the image represented by b.s.(*) BS and return
  an image-info record or #f.  (Don't believe in the colour depth for 
  gifs.)

  (image:info-format info) ==> format | #f
  get the symbolic format id from image-info record INFO 

  (image:info-depth info) ==> depth | #f
  get the colour DEPTH (#bits / pixel) from image-info record INFO 

  (image:info-width/pixel info) ==> w | #f
  (image:info-height/pixel info) ==> h | #f 
  get the pixel width W resp. height H from image-info record INFO

  (image:info-width/dpi info) ==> width | #f
  (image:info-height/dpi info) ==> height | #f 
  get the physical width W resp. height H in dots per inch from etc.


(*) Creating and accessing ``byte streams''
  byte-streams are random-access lazy byte sequences  

  (inport->byte-stream in) ==> bs
  convert the input port IN to a b.s. BS

  (byte-vector->byte-stream bv) ==> bs
  convert the byte-vector bv to a b.s. BS

  (segment-byte-stream bs start end) ==> [bv bs']
  segment the b.s. BS into the first [start:end) bytes (at most) in the 
  byte-vector BV and the following [end:*) bytes in the b.s. BS'.  BV and
  BS' may be empty.
!#

;;; since there is no pre-fab lazy-list facility for s48/scsh but a 
;;; draft srfi, I don't use what's not there and don't set up a 
;;; full-fledged facility myself.  An ad hoc hack shall do for now.

(define (inport->byte-stream in)
  (delay (let ((b (read-byte in)))
           (if (eof-object? b) '()
               (cons b (inport->byte-stream in))))))

(define (byte-vector->byte-stream bv)
  (let ((max (- (byte-vector-length bv) 1)))
    (let loop ((min 0))
      (if (< min max)
          (delay (cons (byte-vector-ref bv min)
                       (loop (+ min 1))))
          (delay '())))))

(define (segment-byte-stream bs start end)
  (assert (<= start end))
  (let* ((bv (make-byte-vector (- end start) 0)))
    (let loop ((i 0) (bs bs))
      (cond ((< i start)
             (let ((bytes (force bs)))
               (if (null? bytes)
                   (values (byte-vector) bs)
                   (loop (+ i 1) (cdr bytes)))))
            ((= i end) (values bv bs))
            ;; start <= i < end
            (else (let ((bytes (force bs)))
                    (if (null? bytes)
                        (values (subsequence bv 0 (- i start)) bs)
                        (begin
                          (byte-vector-set! bv (- i start) (car bytes))
                          (loop (+ i 1) (cdr bytes)))))))
      )))


;; bytes : proper-list(integer) -- think of octets in [0:256)
;; ==> integer                  -- sum bytes[i] * 256^i
;; [ with BYTES reversed (hi-order bytes first), we could use the tail-
;;   recurring left fold, but most of the program is already there now
;;   that I realise the need for quaternary assembly, and I don't want
;;   to go through all AB calls and rearrange the arg lists ]
(define (assemble-bytes . bytes)
  ;; or use kons = (lambda (x y) (bitwise-ior x (arithmetic-shift y 8)))
  (fold-right (lambda (x y) (+ x (* 256 y))) 0 bytes))



(define-record-type :image-info
  (really-make-image-info format
                          depth
                          width/pixel height/pixel
                          width/dpi height/dpi)
  image-info?
  (format image-info:format set-format!)
  (depth image-info:depth set-depth!)   
  (width/pixel image-info:width/pixel set-width/pixel!)   
  (height/pixel image-info:height/pixel set-height/pixel!) 
  (width/dpi image-info:width/dpi set-width/dpi!)       
  (height/dpi image-info:height/dpi set-height/dpi!)     
  )

;; initialize all fields with #f
(define (make-image-info)
  (really-make-image-info #f #f #f #f #f #f))

(define-record-discloser :image-info
  (lambda (r)
    `(image-info ,@(slotlet r 'format image-info:format)
                 ,@(slotlet r 'depth image-info:depth)
                 ,@(slotlet r 'width/pixel image-info:width/pixel)
                 ,@(slotlet r 'height/pixel image-info:height/pixel)
                 ,@(slotlet r 'width/dpi image-info:width/dpi)
                 ,@(slotlet r 'height/dpi image-info:height/dpi)
                 )))

(define (slotlet r tag get)
  (if (get r) `((,tag ,(get r))) '()))


                          
            

;; img : input-port | byte-vector | byte-stream
;; ==> [width height]
;; width, height : integer -- image size in pixels
(define (image-dimension img)
  (let* ((bs (cond ((input-port? img)
                    (inport->byte-stream img))
                   ((byte-vector? img)
                    (byte-vector->byte-stream img))
                   ;; should be a promise
                   (else img)))
         (info (get-image-info bs)))
    (if info
        (values (image-info:width/pixel info)
                (image-info:height/pixel info))
        (error "image-dimension : could not extract W x H"))))
                 

;; img : byte-stream
;; ==> info or #f
(define (get-image-info img)
  (let ((info (make-image-info)))
    (or (fill-info!/gif info img)
        (fill-info!/jpeg info img)
        (fill-info!/png info img))))


(define (fill-info!/gif info bs)
  (receive (bytes foo) (segment-byte-stream bs 0 11)
    (and ;; nothing but gif?
         (= 11 (byte-vector-length bytes))
         (= #x47 (byte-vector-ref bytes 0))
         (= #x49 (byte-vector-ref bytes 1))
         
         ;; magic 8?a
         (= #x46 (byte-vector-ref bytes 2))
         (= #x38 (byte-vector-ref bytes 3))
         (cond ((= #x37 (byte-vector-ref bytes 4))  ; 87a
                (set-format! info 'GIF87A) #t)
               ((= #x39 (byte-vector-ref bytes 4))  ; 89a
                (set-format! info 'GIF89A) #t)
               (else #f))
         (= #x61 (byte-vector-ref bytes 5))

         (begin
           (set-width/pixel! info
                             (assemble-bytes
                              (byte-vector-ref bytes 6)
                              (byte-vector-ref bytes 7)))
           (set-height/pixel! info
                              (assemble-bytes
                               (byte-vector-ref bytes 8)
                               (byte-vector-ref bytes 9)))
           ;; colour depth selon global header -- local palettes (of multi-
           ;; image gifs) may have their own opinion
           (set-depth! info
                       (+ 1 (bitwise-and #x07
                                         (arithmetic-shift
                                          (byte-vector-ref bytes 10) -4))))
           info))))


(define (fill-info!/jpeg info bs)
  (receive (bytes bs) (segment-byte-stream bs 0 2)
    (and
     ;; nothing but jpeg?
     (= 2 (byte-vector-length bytes))
     (= #xff (byte-vector-ref bytes 0))
     (= #xd8 (byte-vector-ref bytes 1))

     (receive/name loop (bytes bs) (segment-byte-stream bs 0 4)
       (if (< 4 (byte-vector-length bytes)) #f
           (let ((marker (assemble-bytes (byte-vector-ref bytes 1)
                                         (byte-vector-ref bytes 0)))
                 (size (assemble-bytes (byte-vector-ref bytes 3)
                                       (byte-vector-ref bytes 2))))

             (cond ((not (= (bitwise-and marker #xff00) #xff00)) #f)

                   ;; APPx
                   ((and (= marker #xffe0)
                         (< size 14)) #f)
                   ((= marker #xffe0)
                    (receive (bytes bs) (segment-byte-stream bs 0 12)
                      (if (< (byte-vector-length bytes) 12) #f
                          (begin
                            (maybe-get-dpi!/jpeg info bytes)
                            (loop (segment-byte-stream
                                   bs (- size 14) (- size 10)))))))
                   ((and (<= #xffc0 marker #xffcf)
                         (not (= marker #xffc4))
                         (not (= marker #xffc8)))
                    (receive (bytes bs) (segment-byte-stream bs 0 6)
                      (if (< 6 (byte-vector-length bytes)) #f
                          (get-most!/jpeg info bytes))))
                   (else 
                    (loop (segment-byte-stream bs (- size 2) (+ size 2))))
                   ))))
     )))


(define (get-most!/jpeg info bytes)
  (set-format! info 'JPEG)
  (set-depth! info
              (* (byte-vector-ref bytes 0)
                 (byte-vector-ref bytes 5)))
  (set-width/pixel! info
                    (assemble-bytes (byte-vector-ref bytes 4)
                                    (byte-vector-ref bytes 3)))
  (set-height/pixel! info
                     (assemble-bytes (byte-vector-ref bytes 2)
                                     (byte-vector-ref bytes 1)))
  info)


;; image-info byte-vector -> any
(define (maybe-get-dpi!/jpeg info bv)
  (define (cm->inch x)
    (floor (/ (* x 254) 100)))
  (and ;; app0 id
       (= #x4a (byte-vector-ref bv 0)) 
       (= #x46 (byte-vector-ref bv 1)) 
       (= #x49 (byte-vector-ref bv 2)) 
       (= #x46 (byte-vector-ref bv 3)) 
       (= #x00 (byte-vector-ref bv 4)) 

       ;; possibly read physical w x h
       (cond ((= 1 (byte-vector-ref bv 7))
              (set-width/dpi! info
                              (assemble-bytes (byte-vector-ref bv 9)
                                              (byte-vector-ref bv 8)))
              (set-height/dpi! info
                               (assemble-bytes (byte-vector-ref bv 11)
                                               (byte-vector-ref bv 10))))
             ((= 2 (byte-vector-ref bv 7))
              (set-width/dpi! info
                              (cm->inch (assemble-bytes
                                         (byte-vector-ref bv 9)
                                         (byte-vector-ref bv 8))))
              (set-height/dpi! info
                              (cm->inch (assemble-bytes
                                         (byte-vector-ref bv 11)
                                         (byte-vector-ref bv 10)))))))) 



(define (fill-info!/png info bs)
  (receive (bytes foo) (segment-byte-stream bs 0 26)
    (and ;; nothing but png?
         (= 26 (byte-vector-length bytes))
         (= #x89 (byte-vector-ref bytes 0))
         (= #x50 (byte-vector-ref bytes 1))
         
         ;; png magic
         (= #x4e (byte-vector-ref bytes 2))
         (= #x47 (byte-vector-ref bytes 3))
         (= #x0d (byte-vector-ref bytes 4))
         (= #x0a (byte-vector-ref bytes 5))
         (= #x1a (byte-vector-ref bytes 6))
         (= #x0a (byte-vector-ref bytes 7))

         ;; get data
         (begin
           (set-format! info 'PNG)
           (set-width/pixel! info
                             (assemble-bytes (byte-vector-ref bytes 19)
                                             (byte-vector-ref bytes 18)
                                             (byte-vector-ref bytes 17)
                                             (byte-vector-ref bytes 16)))
           (set-height/pixel! info
                              (assemble-bytes (byte-vector-ref bytes 23)
                                              (byte-vector-ref bytes 22)
                                              (byte-vector-ref bytes 21)
                                              (byte-vector-ref bytes 20)))
           (set-depth! info
                       (case (byte-vector-ref bytes 25)
                         ((2 6) (* 3 (byte-vector-ref bytes 24)))
                         (else (byte-vector-ref bytes 24))))
           info))))
