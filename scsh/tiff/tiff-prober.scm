#! /usr/local/bin/scsh \
-e tiff-prober -ll sunterlib.scm -o tiff -o tiff-helpers -o endian -s
!#

;****************************************************************************
;				TIFF prober
;
; This code reads a TIFF file and prints out its directory (as well as values
; of a few "important" tags)
; Usage
;	tiff-prober tiff-file1...
; Derived from Oleg Kiselyov's tiff-prober.scm 2.0 2003/10/04 02:35:30
; Changes for the sunterlib
;   procedural wrapper TIFF-PROBER as entry point
;   argv as parameter
;   endian ports moved to endian.scm

(define (tiff-prober argv-s)
  (let
    ((help
      (lambda ()
        (cerr nl nl "print information about TIFF file(s)" nl)
        (cerr nl "Usage")
        (cerr nl "       tiff-prober tiff-file1...")
        (cerr nl nl "Example:")
        (cerr nl "       tiff-prober im1.tiff im2.tiff" nl nl)
        (exit)))
     )
                          ; (car argv-s) is program's name, as usual
    (if (or (null? argv-s) (null? (cdr argv-s)))
      (help))		; at least one argument, besides argv[0], is expected
    (for-each
      (lambda (file-name)
        (cout nl nl "Analyzing TIFF file " file-name "..." nl)
        (let* ((eport (make-endian-port (open-input-file file-name) #t))
               (tiff-dict (read-tiff-file eport))
               (not-spec (lambda () "*NOT SPECIFIED*")))
          (print-tiff-directory tiff-dict (current-output-port))
          (cout nl "image width:    "
            (tiff-directory-get tiff-dict 'TIFFTAG:IMAGEWIDTH not-spec))
          (cout nl "image height:   "
            (tiff-directory-get tiff-dict 'TIFFTAG:IMAGELENGTH not-spec))
          (cout nl "image depth:    "
            (tiff-directory-get tiff-dict 'TIFFTAG:BITSPERSAMPLE not-spec))
          (cout nl "document name:  "
            (tiff-directory-get tiff-dict 'TIFFTAG:DOCUMENTNAME not-spec))
          (cout nl "image description: " nl "  "
            (tiff-directory-get tiff-dict 'TIFFTAG:IMAGEDESCRIPTION not-spec))
          (cout nl "time stamp:     "
            (tiff-directory-get tiff-dict 'TIFFTAG:DATETIME not-spec))
          (cout nl "compression:    "
            (tiff-directory-get-as-symbol tiff-dict
              'TIFFTAG:COMPRESSION not-spec))
          (cout nl nl)))
      (cdr argv-s))) )
