(define-interface afs-fs-interface
  (export
   ((afs-permission afs-permissions) :syntax)
   afs-permissions?
   all-afs-permissions
   make-afs-permissions
   afs-permissions<=?
   afs-permissions->string
   string->afs-permissions
   get-acl
   set-acl!
   add-acl!
   set-fs-command!))

(define-structure afs-fs afs-fs-interface
  (open scheme-with-scsh
	enum-sets
	finite-types
	srfi-1
	srfi-13)
  (files afs-fs))
