#! /usr/local/bin/scsh \
-ll sunterlib.scm -m tiff-testbed -s
!#

;****************************************************************************
;			Validate the TIFF reading package
;
; We test reading of a known TIFF file, print out its directory.
; We also test an internal consistency of the package.
;
; Derived from vtiff.scm 1.1 2003/09/29 20:41:51 oleg

		; Note: some tests below depend on the exact parameters
		; of the following sample file
		; The file is a GNU logo (from http://www.gnu.org)
		; converted from JPG to TIFF

(define sample-tiff-file "gnu-head-sm.tif")

(cerr nl "Verifying the TIFF library" nl)

(cerr nl "Verifying tagdict operations..." nl)
(let ()
  (assert
    (= 256 (tagdict-get-by-name tiff-standard-tagdict 'TIFFTAG:IMAGEWIDTH)))
  (assert (eq? 'TIFFTAG:IMAGEWIDTH
	    (tagdict-get-by-num tiff-standard-tagdict 256)))
  (assert (eq? #f
	    (tagdict-get-by-num tiff-standard-tagdict 65500)))
  (assert (= 5
	    (tagdict-tagval-get-by-name tiff-standard-tagdict
	      'TIFFTAG:COMPRESSION 'LZW)))
  (assert (eq? 'LZW
	    (tagdict-tagval-get-by-num tiff-standard-tagdict
	      'TIFFTAG:COMPRESSION 5)))
  (assert (eq? #f
	    (tagdict-tagval-get-by-num tiff-standard-tagdict
	      'TIFFTAG:COMPRESSION 65500)))

  (let ((ext-dict
	  (tagdict-add-all tiff-standard-tagdict
	    (make-tagdict
	      '((WAupper_left_lat 33004)
		 (WAhemisphere 33003 (North . 1) (South . 2)))))))
    (assert (= 33004 (tagdict-get-by-name  ext-dict 'WAupper_left_lat)))
    (assert (eq? 'WAupper_left_lat
	      (tagdict-get-by-num ext-dict 33004)))
    (assert (eq? 'TIFFTAG:PHOTOMETRIC (tagdict-get-by-num ext-dict 262)))
    (assert (eq? #f
	      (tagdict-tagval-get-by-num ext-dict 'WAupper_left_lat 0)))
    (assert (= 1
	      (tagdict-tagval-get-by-name ext-dict 'WAhemisphere 'North)))
    (assert (eq? 'South
	      (tagdict-tagval-get-by-num ext-dict 'WAhemisphere 2))))
)

(define (test-dictionary-consistency tiff-dict)
  (cerr nl "Verifying the consistency of dictionary operations ..." nl)
  (assert (tiff-directory? tiff-dict))
  (assert (positive? (tiff-directory-size tiff-dict))
          (not (tiff-directory-empty? tiff-dict)))
  (assert (=
	    (tiff-directory-get tiff-dict 'TIFFTAG:IMAGEWIDTH)
	    (tiff-directory-get tiff-dict 256)))
  (assert (eq? #f (tiff-directory-get tiff-dict 65500)))
  (let ((not-given (list 'not-given)))
    (assert (eq? not-given (tiff-directory-get tiff-dict 65500
			     (lambda () not-given)))))
  (let ((size (tiff-directory-size tiff-dict)))
    (call-with-values
      (lambda ()
	(tiff-directory-fold-left tiff-dict
	  (lambda (el count) (values #t (+ 1 count))) 0))
      (lambda (size-via-fold)
	(assert (= size size-via-fold)))))
  (let*-values
    (((len) (tiff-directory-get tiff-dict 'TIFFTAG:IMAGELENGTH))
     ((len-via-fold prev-count)
       (tiff-directory-fold-left tiff-dict
	 (lambda (dir-entry found prev-count)
	   (if (= (tiff-dir-entry-tag dir-entry) 257)
	     (values #f (force (tiff-dir-entry-value dir-entry))
	       prev-count)		; and terminate now
	     (values #t #f (+ 1 prev-count))))
	 #f 0)))
    (assert (= len len-via-fold)
      (< 0 prev-count (tiff-directory-size tiff-dict))))
)

(define (test-known-values-from-dict tiff-dict)
  (cerr nl "Getting sample data from the dictionary ")
  (let
    ((known-values
       '((TIFFTAG:IMAGEWIDTH . 129)
	 (TIFFTAG:IMAGELENGTH . 122)
	 (TIFFTAG:BITSPERSAMPLE . 8)
	 (TIFFTAG:IMAGEDESCRIPTION . "JPEG:gnu-head-sm.jpg 129x122")
	 (TIFFTAG:COMPRESSION . 1)
	 (TIFFTAG:SAMPLESPERPIXEL . 1)
	 (TIFFTAG:STRIPBYTECOUNTS . 15738) ; the product of width and length
	 (TIFFTAG:XRESOLUTION . 72)
	 (TIFFTAG:CLEANFAXDATA . #f))))
    (for-each
      (lambda (tag-val)
	(cerr "Tag " (car tag-val) "...")
	(let ((real (tiff-directory-get tiff-dict (car tag-val))))
	  (cerr real nl)
	  (assert (equal? real (cdr tag-val)))))
      known-values
      ))
  (assert (eq? 'NONE
	    (tiff-directory-get-as-symbol tiff-dict
	      'TIFFTAG:COMPRESSION)))
)


(define (test-reading-pixel-matrix tiff-dict eport)
  (cerr nl "Reading the pixel matrix and spot-checking it ...")
  ; Make sure we can handle this particular TIFF image
  ; No compression
  (assert (eq? 'NONE
	    (tiff-directory-get-as-symbol tiff-dict
	      'TIFFTAG:COMPRESSION)))
  (assert (= 1 (tiff-directory-get tiff-dict 'TIFFTAG:SAMPLESPERPIXEL)))
  (assert (= 8 (tiff-directory-get tiff-dict 'TIFFTAG:BITSPERSAMPLE)))

  (let*
    ((ncols  (tiff-directory-get tiff-dict 'TIFFTAG:IMAGEWIDTH))
     (_      (assert (number? ncols) (positive? ncols)))
     (nrows  (tiff-directory-get tiff-dict 'TIFFTAG:IMAGELENGTH))
     (_      (assert (number? nrows) (positive? nrows)))
     (rows-per-strip (tiff-directory-get tiff-dict 'TIFFTAG:ROWSPERSTRIP
		       (lambda () nrows)))
     (_      (assert (positive? rows-per-strip)))
     (strip-offsets (tiff-directory-get tiff-dict 'TIFFTAG:STRIPOFFSETS
		      (lambda () (error "STRIPOFFSETS must be present!"))))
      ; make it a u32vector
     (strip-offsets
       (cond
	 ((u32vector? strip-offsets) strip-offsets)
	 ((u16vector? strip-offsets)
	   (list->u32vector (u16vector->list strip-offsets)))
	 (else (u32vector strip-offsets))))
     (image-size (* nrows ncols))
     (strip-size (* rows-per-strip ncols))
     (image (make-u8vector image-size 0))
    )
    (cerr nl "Loading the image matrix of the size " image-size
      " bytes...")
    (let outer ((strip 0) (i 0))
      (if (>= strip (u32vector-length strip-offsets)) #f
	(let ((i-end (min (+ i strip-size) image-size)))
	  (endian-port-setpos eport (u32vector-ref strip-offsets strip))
	  (let inner ((i i))
	    (if  (>= i i-end) (outer (++ strip) i)
	      (begin
		(u8vector-set! image i (endian-port-read-int1 eport))
		(inner (++ i))))))))
    (assert (= 255 (u8vector-ref image 0))
            (= 248 (u8vector-ref image 17)))
    ;(display image)
    ))



(cerr nl "Reading the sample TIFF file " sample-tiff-file "..." nl)
(let* ((eport (make-endian-port (open-input-file sample-tiff-file) #t))
	(tiff-dict (read-tiff-file eport)))
  (print-tiff-directory tiff-dict (current-output-port))
  (test-known-values-from-dict tiff-dict)
  (test-dictionary-consistency tiff-dict)
  (test-reading-pixel-matrix tiff-dict eport)
)


(cerr nl "All tests passed" nl)
