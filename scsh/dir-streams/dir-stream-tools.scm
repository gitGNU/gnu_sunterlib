(define (dir-stream-disc-usage ds)
   (dir-stream-fold-right ds (lambda (fso sum subdirs) 
                               (list (fs-object-file-name fso)
                                     (apply + sum (map cadr subdirs))
                                     subdirs))
 			 (lambda (fso accu)
 			   (+ accu (file-info:size (fs-object-info fso))))
 			 0
 			 cons
 			 '()))

(define (dir-stream-display ds)
  (dir-stream-for-each ds display display))
