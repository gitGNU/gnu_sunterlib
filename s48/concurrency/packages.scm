(define-structure semaphores semaphores-interface
  (open scheme
	locks
	with-lock
	define-record-types)
  (files semaphore))

(define-structure with-lock with-lock-interface
  (open scheme 
	locks)
  (files with-lock))