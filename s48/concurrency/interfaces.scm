(define-interface semaphores-interface
  (export make-semaphore
	  semaphore-post
	  semaphore-wait
	  with-semaphore-posted))

(define-interface with-lock-interface
  (export with-lock))