(define-interface srfi-34-interface
  (export
   raise
   with-exception-handler
   with-exception-handlers
   (guard :syntax)))

(define-interface srfi-35-interface
  (export
   make-condition-type
   condition-type?
   make-condition
   condition?
   condition-has-type?
   condition-ref
   make-compound-condition
   extract-condition
   (define-condition-type :syntax)
   (condition :syntax)
   &condition
   &message message-condition? condition-message
   &serious serious-condition?
   &error error?))

(define-interface srfi-34-restart-interface
  (export
   (raise-restartable :syntax)
   (restart :syntax)))
