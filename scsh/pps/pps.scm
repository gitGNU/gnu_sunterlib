;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define-record-type process-info :process-info
  (really-make-process-info pid ppid logname 
			    real-uid effective-uid saved-set-uid 
			    real-gid effective-gid saved-set-gid
			    time %cpu tty executable command-line)
  process-info?
  (pid process-info-pid)
  (ppid process-info-ppid)
  (logname process-info-logname)
  (real-uid process-info-real-uid)
  (effective-uid process-info-effective-uid)
  (saved-set-uid process-info-saved-set-uid)
  (real-gid process-info-real-gid)
  (effective-gid process-info-effective-gid)
  (saved-set-gid process-info-saved-set-gid)  
  (time process-info-time)
  (%cpu process-info-%cpu)
  (tty process-info-tty)
  (executable process-info-executable)
  (command-line process-info-command-line))

(define-record-discloser :process-info
  (lambda (pi)
    (list 'pi (process-info-pid pi) (process-info-executable pi) (process-info-logname pi))))

(define (make-process-info-with-restlist 
	 ps-functions
	 pid ppid logname 
	 real-uid effective-uid saved-set-uid 
	 real-gid effective-gid saved-set-gid
	 time %cpu tty executable . command-line)
  (really-make-process-info 
   (string->number pid) (string->number ppid) logname 
   (string->number real-uid) (string->number effective-uid) 
   (string->number saved-set-uid)
   (string->number real-gid) (string->number effective-gid)
   (string->number saved-set-gid)
   ((ps-time->seconds ps-functions) time)
   (string->number %cpu)
   tty executable command-line))

(define *os-pss* '())

(define (add-ps! ps-functions)
  (set! *os-pss* (cons ps-functions *os-pss*)))

(define (pps)
  (let ((os (uname:os-name (uname)))) 
    (let lp ((os-pss *os-pss*))
      (if (null? *os-pss*)
	  (error "ps not defined for " os)
	  (let ((next (car os-pss)))
	    (if (string=? os (ps-uname-string next))
		(call-ps next)
		(lp (cdr os-pss))))))))

(define (call-ps ps-functions)
  (map (lambda (line)
	 (apply make-process-info-with-restlist 
		ps-functions 
		((ps-line-splitter ps-functions) line)))
       ((ps-command ps-functions))))

(define-record-type ps-functions :ps-functions
  (really-make-ps-functions uname-string command time->seconds line-splitter)
  ps-functions?
  (uname-string ps-uname-string)
  (command ps-command)
  (time->seconds ps-time->seconds)
  (line-splitter ps-line-splitter))

(define (make-ps-functions uname-string command time->seconds . maybe-line-splitter)
  (really-make-ps-functions uname-string 
			    command time->seconds
			    (if (null? maybe-line-splitter)
				(sloppy-suffix-splitter)
				(car maybe-line-splitter))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FreeBSD

(define (fbsd-ps-command)
  (let ((res (run/strings 
	      (ps -axww 
		  ;; uses rgid instead of gid
		  "-opid,ppid,user,ruid,uid,svuid,rgid,rgid,svgid,time,%cpu,tty,ucomm,command"))))
    (if (null? res)
	(error "ps failed")
	(cdr res))))

(define fbsd-time->seconds		; hmm, what does the output look like for older processes??
  (let ((short-rx (rx (: (submatch (+ digit)) #\: 
			 (submatch (: digit digit)) #\. 
			 (submatch (: digit digit))))))
    (lambda (time)
      (let ((match-data (regexp-search short-rx time)))
	(if match-data
	    (+ (* 60 60 (string->number (match:substring match-data 1)))
	       (* 60 (string->number (match:substring match-data 2)))
	       (quotient (string->number (match:substring match-data 3)) 50))
	    0)))))

(define fbsd-ps (make-ps-functions "FreeBSD" fbsd-ps-command fbsd-time->seconds))
(add-ps! fbsd-ps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Darwin

(define (darwin-ps-command)
  (let ((res (run/strings 
	      (ps -axww 
		  ;; uses rgid instead of gid
		  "-opid,ppid,user,ruid,uid,svuid,rgid,svgid,time,%cpu,tty,ucomm,command"))))
    (if (null? res)
	(error "ps failed")
	(cdr res))))

(define (darwin-line-splitter line)
  (let ((line-as-list ((sloppy-suffix-splitter) line)))
    (append (take line-as-list 7)
	    (list (list-ref line-as-list 7))
	    (drop line-as-list 7))))

(define darwin-ps (make-ps-functions "Darwin" 
				     darwin-ps-command 
				     fbsd-time->seconds
				     darwin-line-splitter))
(add-ps! darwin-ps)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linux
(define (linux-ps-command)
  (let ((res (run/strings 
	      (ps -A ;axww 
		  "-opid,ppid,user,ruid,uid,svuid,rgid,gid,svgid,time,%cpu,tty,ucomm,command"))))
    (if (null? res)
	(error "ps failed")
	(cdr res))))

(define linux-time->seconds		; hmm, what does the output look like for older processes??
  (let ((short-rx (rx (: (submatch (+ digit)) #\: 
			 (submatch (: digit digit)) #\: 
			 (submatch (: digit digit))))))
    (lambda (time)
      (let ((match-data (regexp-search short-rx time)))
	(if match-data
	    (+ (* 60 60 (string->number (match:substring match-data 1)))
	       (* 60 (string->number (match:substring match-data 2)))
	       (string->number (match:substring match-data 3)))
	    0)))))

(define linux-ps (make-ps-functions "Linux" linux-ps-command linux-time->seconds))
(add-ps! linux-ps)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AIX

(define (aix-ps-command)
  (let ((res (run/strings 
	      (ps -A
		  ;; uses ruid/rgid instead of svuid/svgid
		  -opid -oppid -ouser -oruid -ouid -oruid -orgid -ogid -orgid -otime -opcpu -otty -oucomm -oargs))))
    (if (null? res)
	(error "ps failed")
	(cdr res))))

(define aix-time->seconds
  (let ((short-rx (rx (: (? (submatch (** 1 3 digit)) #\-)
			 (submatch (** 1 2 digit)) #\:
			 (submatch (: digit digit)) #\: 
			 (submatch (: digit digit))))))
    (lambda (time)
      (let ((match-data (regexp-search short-rx time)))
	(if match-data
	    (+ 
	     (if (match:substring match-data 1)
		 (* 24 60 60 (string->number (match:substring match-data 1)))
		 0)
	     (* 60 60 (string->number (match:substring match-data 2)))
	     (* 60 (string->number (match:substring match-data 3)))
	     (string->number (match:substring match-data 4)))
	    (error "cannot parse time" time))))))

(define aix-ps (make-ps-functions "AIX" aix-ps-command aix-time->seconds))
(add-ps! aix-ps)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solaris

(define (solaris-ps-command)
  (let ((res (run/strings 
	      (ps -A
		  ;; uses ruid/rgid instead of svuid/svgid
		  -opid -oppid -ouser -oruid -ouid -oruid -orgid -ogid -orgid -otime -opcpu -otty -ocomm -oargs))))
    (if (null? res)
	(error "ps failed")
	(cdr res))))

(define solaris-time->seconds
  (let ((short-rx (rx (: (? (submatch (** 1 3 digit)) #\-)
			 (? (submatch (** 1 2 digit)) #\:)
			 (submatch (** 1 2 digit)) #\: 
			 (submatch (: digit digit))))))
    (lambda (time)
      (let ((match-data (regexp-search short-rx time)))
	(if match-data
	    (+ 
	     (if (match:substring match-data 1)
		 (* 24 60 60 (string->number (match:substring match-data 1)))
		 0)
	     (if (match:substring match-data 2)
		 (* 60 60 (string->number (match:substring match-data 2)))
		 0)
	     (* 60 (string->number (match:substring match-data 3)))
	     (string->number (match:substring match-data 4)))
	    (error "cannot parse time" time))))))

(define solaris-ps (make-ps-functions "SunOS" solaris-ps-command solaris-time->seconds))
(add-ps! solaris-ps)
