;;; This file is part of the Scheme Untergrund Library.

;;; Copyright (c) 2002-2003 by Martin Gasbichler.
;;; For copyright information, see the file COPYING which comes with
;;; the distribution.

(define-enumerated-type afs-permission :afs-permission
  afs-permission?
  the-afs-permissions
  afs-permission-name
  afs-permission-index
  (read
   list
   insert
   delete
   write
   lock
   administer))

(define-enum-set-type afs-permissions :afs-permissions
  afs-permissions?
  make-afs-permissions
  afs-permission afs-permission? the-afs-permissions afs-permission-index)

(define (char->afs-permission c)
  (case c
    ((#\r) (afs-permission read))
    ((#\l) (afs-permission list))
    ((#\i) (afs-permission insert))
    ((#\d) (afs-permission delete))
    ((#\w) (afs-permission write))
    ((#\k) (afs-permission lock))
    ((#\a) (afs-permission administer))
    (else (error "wrong char in char->afs-permission" c))))

(define (afs-permission->char afs-perm)
  (cond ((eq? afs-perm (afs-permission read)) #\r)
	((eq? afs-perm (afs-permission list)) #\l)
	((eq? afs-perm (afs-permission insert)) #\i)
	((eq? afs-perm (afs-permission delete)) #\d)
	((eq? afs-perm (afs-permission write)) #\w)
	((eq? afs-perm (afs-permission lock)) #\k)
	((eq? afs-perm (afs-permission administer)) #\a)
	(else (error "unknown permission" afs-perm))))

(define (string->afs-permissions s)
  (string-fold (lambda (e accu)
		 (enum-set-union accu (make-afs-permissions (list (char->afs-permission e)))))
	       (make-afs-permissions '())
	       s))

(define (afs-permissions->string afs-perms)
  (if (null? (enum-set->list afs-perms))
      "none"
      (fold (lambda (perm s)
	      (if (enum-set-member? afs-perms perm)
		  (string-append s (string (afs-permission->char perm)))
		  s))
	    ""
	    (vector->list the-afs-permissions))))
  
(define all-afs-permissions
  (make-afs-permissions (map char->afs-permission (string->list "rlidwka"))))

(define (afs-permissions<=? p1 p2)
  (enum-set=? (enum-set-union p1 p2) p2))

;; access control lists: lists of pairs of user name and
;; afs-permissions

(define (get-acl dir)
  (let* ((output (run/strings (,fs la ,dir))))
    (map (lambda (s)
	   (apply (lambda (user al)
		    (cons user (string->afs-permissions al)))
		  ((field-splitter) s)))
	 (cddr output)))) ; TODO add sanity check

(define (acl-users acl)
  (map car acl))

(define (acl->strings acl)
  (apply append
	 (map (lambda (user.afs-perms)
		(list (car user.afs-perms) 
		      (afs-permissions->string (cdr user.afs-perms))))
	      acl)))

(define (arla-set-access-control-list-for-user! dir user afs-perms)
  (run (,fs sa ,dir ,user ,(afs-permissions->string afs-perms))))

(define (arla-set-access-control-list! dir acl)
  (for-each (lambda (user.afs-perms)
	      (arla-set-access-control-list-for-user! dir (car user.afs-perms) (cdr user.afs-perms)))
	    acl))

(define (set-acl! dir acl)
  (run (,fs setacl ,dir -acl ,@(acl->strings acl) -clear)))

(define (add-acl! dir acl)
  (run (,fs setacl ,dir -acl ,@(acl->strings acl))))

(define fs "fs")

(define (set-fs-command! the-fs)
  (set! fs the-fs))
