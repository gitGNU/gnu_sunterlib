(define-structure semaphores 
  (open scheme
	locks
	with-lock
	define-record-types)
  (files semaphore))

(define-structure with-lock with-lock-interface
  (open scheme 
	locks)
  (files with-lock))