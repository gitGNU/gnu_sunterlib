
(define-interface trans-ids-interface
  (export enter-cr! leave-cr!
	  leave-cr-and-block!
	  trans-id?
	  make-trans-id
	  cr-trans-id-wait cr-trans-id-wakeup cr-maybe-trans-id-wakeup
	  trans-id-thread-uid trans-id-cancelled?))

(define-interface rendezvous-interface
  (export always-rv never-rv
	  guard with-nack choose wrap
	  sync
	  select))

(define-interface make-rendezvous-interface
  (export make-blocked make-enabled make-base))

(define-interface rendezvous-channels-interface
  (export make-channel
	  channel?
	  send-rv send
	  receive-rv receive))

(define-interface rendezvous-async-channels-interface
  (export make-async-channel
	  async-channel?
	  send-async
	  receive-async-rv
	  receive-async))

(define-interface rendezvous-placeholders-interface
  (export make-placeholder
	  placeholder?
	  placeholder-value
	  placeholder-set!
	  placeholder-value-rv))

(define-interface rendezvous-jars-interface
  (export make-jar
	  jar?
	  jar-take
	  jar-put!
	  jar-take-rv))

