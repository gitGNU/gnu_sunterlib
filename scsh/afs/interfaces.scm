(define-interface afs-fs-interface
  (export
   ((afs-permission afs-permissions) :syntax)
   afs-permissions?
   all-afs-permissions
   make-afs-permissions
   afs-permissions<=?
   get-acl
   set-acl!
   add-acl!
   set-fs-command!))
