(define-interface afs-fs-interface
  (export
   ((afs-permission afs-permissions) :syntax)
   all-afs-permissions
   get-acl
   set-acl!
   add-acl!
   set-fs-command!))