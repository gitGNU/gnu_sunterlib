(define-structure trans-ids trans-ids-interface
  (open scheme
	srfi-9 big-util
	threads threads-internal interrupts
	locks placeholders)
  (files trans-id))

(define-structures ((rendezvous rendezvous-interface)
		    (make-rendezvous make-rendezvous-interface))
  (open scheme
	srfi-9 (subset define-record-types (define-record-discloser))
	trans-ids
	threads threads-internal
	big-util
	(subset util (unspecific)))
  (files rendezvous))

(define-structure rendezvous-channels rendezvous-channels-interface
  (open scheme
	srfi-9
	trans-ids rendezvous make-rendezvous
	queues
	big-util
	(subset util (unspecific)))
  (files channel))

(define-structure rendezvous-async-channels rendezvous-async-channels-interface
  (open scheme
	rendezvous
	rendezvous-channels
	threads
	queues
	srfi-9)
  (files async-channels))

(define-structure rendezvous-placeholders rendezvous-placeholders-interface
  (open scheme
	srfi-9 (subset define-record-types (define-record-discloser))
	trans-ids rendezvous make-rendezvous
	queues
	signals
	(subset util (unspecific)))
  (files placeholder))

(define-structure rendezvous-jars rendezvous-jars-interface
  (open scheme
	srfi-9 (subset define-record-types (define-record-discloser))
	trans-ids rendezvous make-rendezvous
	queues
	signals
	(subset util (unspecific)))
  (files jar))
