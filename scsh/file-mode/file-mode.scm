; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees. See file COPYING.


(define-record-type file-mode :file-mode
  (really-make-file-mode value)
  file-mode?
  (value file-mode->integer))

(define-record-discloser :file-mode
  (lambda (file-mode)
    (list 'file-mode
	  (string-append "0"
			 (number->string (file-mode->integer file-mode)
					 8)))))


; STUFF can be a number (#o644), a string ("rwxr--r--"), or ???
; Or should there be another macro?
;
; For now it has to be a number

(define (integer->file-mode stuff)
  (cond ((and (integer? stuff)
	      (<= 0 stuff)
	      (<= stuff #o7777))
	 (really-make-file-mode stuff))
	(else
	 (error "invalid argument to integer->file-mode" stuff))))

; Arithmetic

(define (file-mode+ . modes)
  (do ((i 0 (bitwise-ior i (file-mode->integer (car modes))))
       (modes modes (cdr modes)))
      ((null? modes)
       (integer->file-mode i))))

(define (file-mode- mode1 mode2)
  (integer->file-mode (bitwise-and (file-mode->integer mode1)
				   (bitwise-not (file-mode->integer mode2)))))

; Comparisons

(define (file-mode=? mode1 mode2)
  (= (file-mode->integer mode1)
     (file-mode->integer mode2)))

(define (file-mode<=? mode1 mode2)
  (= 0 (bitwise-and (file-mode->integer mode1)
		    (bitwise-not (file-mode->integer mode2)))))

(define (file-mode>=? mode1 mode2)
  (file-mode<=? mode2 mode1))

; Names for various permissions

(define-syntax file-mode
  (lambda (e r c)
    (let* ((names '((set-uid     . #o4000)
		    (set-gid     . #o2000)

		    (owner-read  . #o0400)
		    (owner-write . #o0200)
		    (owner-exec  . #o0100)
		    (owner       . #o0700)

		    (group-read  . #o0040)
		    (group-write . #o0020)
		    (group-exec  . #o0010)
		    (group       . #o0070)

		    (other-read  . #o0004)
		    (other-write . #o0002)
		    (other-exec  . #o0001)
		    (other       . #o0007)

		    (read        . #o0444)
		    (write       . #o0222)
		    (exec        . #o0111)
		    (all	 . #o0777)))
	   (lookup (lambda (name)
		     (let loop ((names names))
		       (cond ((null? names)
			      #f)
			     ((c name (caar names))
			      (cdar names))
			     (else
			      (loop (cdr names))))))))
      (if (or (null? (cdr e))
	      (not (pair? (cdr e))))
	  e
	  (let loop ((todo (cdr e)) (mask 0))
	    (cond ((null? todo)
		   `(,(r 'integer->file-mode) ,mask))
		  ((and (pair? todo)
			(lookup (car todo)))
		   => (lambda (i)
			(loop (cdr todo) (bitwise-ior i mask))))
		  (else
		   e)))))))
