(define-interface dir-streams-interface
  (export dir-stream-from-dir-name
          dir-stream?
          dir-stream-files-stream
          dir-stream-subdir-stream
          dir-stream-info

	  dir-stream-for-each
	  dir-stream-map
	  dir-stream-filter
	  dir-stream-filter-map
	  dir-stream-fold-right

          fs-object?
	  fs-object-parent
	  fs-object-name
	  fs-object-info
	  fs-object-file-name))

(define-interface dir-stream-predicates-interfaces
  (export fs-object-size-less-than?
	  fs-object-size-greater-than?
	  days->seconds
	  hours->seconds
	  minutes->seconds
	  fs-object-last-modified-in?
	  fs-object-last-accessed-in?
	  fs-object-created-in?
	  fs-object-name-matches?
	  ds-object-file-name-matches?))

(define-interface streams-interface
  (export the-empty-stream
	  make-empty-stream
	  make-stream
	  make-stream-lazily
	  (stream :syntax)
	  stream-empty?
	  stream-head
	  stream-tail
	  stream-map
	  stream-zip-with
	  stream-for-each
	  stream-filter
	  stream-filter-map
	  stream-unfold
	  stream-transform
	  stream-take
	  stream-drop
	  stream-fold-right
	  stream-fold-right-lazily
	  stream-prepend
	  list->stream
	  stream-from
	  stream-from-to
	  stream-from-then
	  stream-from-then-to
	  stream-ref
	  stream-iterate
	  stream-cycle
	  stream-take-while
	  stream-drop-while))

(define-structure dir-streams dir-streams-interface
  (open scheme-with-scsh
	handle
	conditions
	define-record-types
	let-opt
	records
	streams)
  (files dir-stream))

(define-structure dir-stream-predicates dir-stream-predicates-interfaces
  (open
   scheme-with-scsh
   dir-streams)
  (files dir-stream-predicates))

(define-structure streams
  streams-interface
  (open scheme
	signals)
  (files stream))
